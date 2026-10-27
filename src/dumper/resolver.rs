use std::collections::{HashMap, HashSet};
use std::{fmt, io, iter, mem, ptr};

use anyhow::{anyhow, bail, ensure, Context};
use red4ext_rs::log;
use red4ext_rs::types::{
    CName, Class, Curve, Enum, IScriptable, ISerializable, RaRef, RedArray, Ref, ResourceRef,
    TaggedType, Type, TypeKind, ValuePtr, WeakRef,
};

use super::{sanitize_field, Dumper};
use crate::layout::{size_compatible_alignment, Layout, StructLayout};

#[derive(Debug)]
pub(super) struct LayoutResolver<'d, 'r> {
    dumper: &'d Dumper<'r>,
    classes: HashMap<CName, ClassPlan<'r>>,
    visiting: HashSet<CName>,
    pending_array_elements: Vec<&'r Type>,
    queued_array_elements: HashSet<*const Type>,
}

impl<'d, 'r> LayoutResolver<'d, 'r> {
    pub(super) fn new(dumper: &'d Dumper<'r>) -> Self {
        Self {
            dumper,
            classes: HashMap::new(),
            visiting: HashSet::new(),
            pending_array_elements: Vec::new(),
            queued_array_elements: HashSet::new(),
        }
    }

    fn type_layout(&mut self, typ: &'r Type) -> anyhow::Result<Layout> {
        let layout = if let Some(mapped) = self.dumper.known_types.get(&ptr::from_ref(typ)) {
            mapped.layout
        } else {
            match typ.tagged() {
                TaggedType::Name => Layout::of::<CName>(),
                TaggedType::Class(class) => {
                    ensure!(
                        !self.dumper.excluded_classes.contains(&class.name()),
                        "no Rust layout for excluded class {}",
                        class.name()
                    );
                    self.class_plan(class)?.layout
                }
                TaggedType::Enum(enum_) => enum_layout(enum_)?,
                TaggedType::BitField(bf) => Layout::bytes(bf.byte_size().into()),
                TaggedType::Array(array) => {
                    // Validate strides after class plans are complete, so a class
                    // can contain RedArray<Self> without an inline-layout cycle.
                    let element = array.element_type();
                    if self.queued_array_elements.insert(ptr::from_ref(element)) {
                        self.pending_array_elements.push(element);
                    }
                    Layout::of::<RedArray<u8>>()
                }
                TaggedType::StaticArray(array) => self
                    .type_layout(array.element_type())?
                    .static_array(array.size())?,
                TaggedType::NativeArray(array) => self
                    .type_layout(array.element_type())?
                    .array(array.size())?,
                TaggedType::FixedArray(array) => self
                    .type_layout(array.element_type())?
                    .array(fixed_array_len(typ)?)?,
                TaggedType::Ref(_) => Layout::of::<Ref<IScriptable>>(),
                TaggedType::WeakRef(_) => Layout::of::<WeakRef<IScriptable>>(),
                TaggedType::ResourceRef(_) => Layout::of::<ResourceRef<u8>>(),
                TaggedType::RaRef(_) => Layout::of::<RaRef<u8>>(),
                TaggedType::Curve(_) => Layout::of::<Curve<u8>>(),
                _ => bail!("unsupported RTTI type {} ({:?})", typ.name(), typ.kind()),
            }
        };
        ensure!(
            layout.size == typ.size(),
            "Rust size {} for {} differs from RTTI size {}",
            layout.size,
            typ.name(),
            typ.size()
        );
        Ok(layout)
    }

    pub(super) fn validate_all(&mut self) -> anyhow::Result<()> {
        let mut errors = Vec::new();
        for name in self.dumper.rust_type_names.keys() {
            if self.dumper.excluded_classes.contains(name) {
                continue;
            }
            let result = (|| -> anyhow::Result<()> {
                let typ = self
                    .dumper
                    .rtti
                    .get_type(*name)
                    .ok_or_else(|| anyhow!("could not resolve type {name}"))?;
                if let TaggedType::Class(class) = typ.tagged() {
                    self.class_plan(class)?;
                }
                Ok(())
            })();
            if let Err(error) = result {
                errors.push(error);
            }
        }
        self.validate_array_elements(&mut errors);
        report_layout_errors(&errors)
    }

    fn validate_array_elements(&mut self, errors: &mut Vec<anyhow::Error>) {
        // Types are deduplicated on enqueue, including recursive array elements.
        // Retain the actual type because anonymous types cannot be resolved by name.
        while let Some(typ) = self.pending_array_elements.pop() {
            let result = self.type_layout(typ).with_context(|| {
                format!(
                    "validating array element stride for {} ({:?})",
                    typ.name(),
                    typ.kind()
                )
            });
            if let Err(error) = result {
                errors.push(error);
            }
        }
    }

    pub(super) fn class_plan(&mut self, class: &'r Class) -> anyhow::Result<&ClassPlan<'r>> {
        let name = class.name();
        if !self.classes.contains_key(&name) {
            ensure!(
                self.visiting.insert(name),
                "recursive inline class layout for {name}"
            );
            let plan = self
                .build_class_plan(class)
                .with_context(|| format!("laying out class {name}"));
            self.visiting.remove(&name);
            self.classes.insert(name, plan?);
        }
        Ok(&self.classes[&name])
    }

    fn build_class_plan(&mut self, class: &'r Class) -> anyhow::Result<ClassPlan<'r>> {
        let alignment_override = self.dumper.alignment_overrides.get(&class.name()).copied();
        // Script holders are separate storage: object alignment does not describe
        // them. CClass defaults allocation alignment to 4 even for smaller value
        // types; retain it only as far as native storage size permits.
        let is_holder = class.is_class() && !class.flags().is_native();
        let min_alignment = if is_holder {
            1
        } else {
            size_compatible_alignment(
                class.size(),
                alignment_override.unwrap_or(class.alignment()),
            )?
        };
        let mut layout = StructLayout::new(min_alignment)?;
        let mut fields = Vec::new();
        let mut names = HashSet::new();
        let mut padding_fields = 0;
        let mut bases = iter::once(class)
            .chain(class.base_iter())
            .take_while(|c| c.flags().is_native() == class.flags().is_native())
            .collect::<Vec<_>>();
        bases.reverse();
        let (bases, known_base) = match &bases[..] {
            [_, base, rem @ ..] if base.name() == CName::new("IScriptable") => {
                (rem, Some(KnownBase::IScriptable))
            }
            [base, rem @ ..] if base.name() == CName::new("ISerializable") => {
                (rem, Some(KnownBase::ISerializable))
            }
            _ => (&bases[..], None),
        };
        if let Some(base) = known_base {
            let (rust_type, base_layout) = match base {
                KnownBase::IScriptable => ("IScriptable", Layout::of::<IScriptable>()),
                KnownBase::ISerializable => ("ISerializable", Layout::of::<ISerializable>()),
            };
            layout.push(0, base_layout)?;
            names.insert("base".into());
            fields.push(FieldPlan {
                name: "base".into(),
                rust_type: RustType::Named(rust_type),
                offset: 0,
                padding: None,
            });
        }

        let props = bases
            .iter()
            .flat_map(|base| {
                self.dumper
                    .class_properties
                    .get(&base.name())
                    .map(Vec::as_slice)
                    .unwrap_or_default()
            })
            .enumerate()
            // Reserve real field names before allocating padding identifiers.
            .map(|(i, &prop)| {
                let name = unique_name(sanitize_field(prop.name().as_str(), i), &mut names);
                (prop, name)
            })
            .collect::<Vec<_>>();
        for (prop, name) in props {
            let offset = prop.value_offset();
            let resolved = (|| -> anyhow::Result<_> {
                let field = ResolvedType {
                    rust_type: {
                        self.dumper
                            .write_type_name(prop.type_(), &mut TypeNameSink)?;
                        RustType::Rtti(prop.type_())
                    },
                    layout: self.type_layout(prop.type_())?,
                }
                .at_offset(offset, matches!(prop.type_().tagged(), TaggedType::Enum(_)));
                let padding = layout.push(offset, field.layout)?;
                Ok((field.rust_type, padding))
            })()
            .with_context(|| format!("property {} at {offset:#X}", prop.name()))?;
            let (rust_type, padding) = resolved;
            let padding = Padding::new(padding, &mut padding_fields, &mut names);
            fields.push(FieldPlan {
                name,
                rust_type,
                offset,
                padding,
            });
        }
        let (layout, tail) = finish_class_layout(layout, class.properties_size(), is_holder)?;
        let tail_padding = Padding::new(tail, &mut padding_fields, &mut names);
        Ok(ClassPlan {
            fields,
            tail_padding,
            layout,
            known_base,
        })
    }
}

pub(super) fn report_layout_errors(errors: &[anyhow::Error]) -> anyhow::Result<()> {
    for (index, error) in errors.iter().enumerate() {
        log::error!(
            "Layout validation error {}/{}: {error:#}",
            index + 1,
            errors.len()
        );
    }
    ensure!(
        errors.is_empty(),
        "layout validation failed with {} error(s); see preceding log entries",
        errors.len()
    );
    Ok(())
}

pub(super) fn finish_class_layout(
    layout: StructLayout,
    size: u32,
    is_holder: bool,
) -> anyhow::Result<(Layout, u32)> {
    let (layout, tail) = layout.finish(size)?;
    ensure!(
        is_holder || layout.size == size,
        "Rust class size {} (alignment {}) differs from RTTI size {size}",
        layout.size,
        layout.alignment
    );
    Ok((layout, tail))
}

pub(super) fn fixed_array_len(typ: &Type) -> anyhow::Result<u32> {
    ensure!(
        typ.kind() == TypeKind::FixedArray,
        "expected a fixed array type"
    );
    let array = typ
        .as_array()
        .context("fixed array has no array metadata")?;
    // Fixed-array length is type-level metadata; its getter does not access an instance.
    Ok(unsafe { array.length(mem::transmute::<*mut (), ValuePtr>(ptr::null_mut())) })
}

fn enum_layout(enum_: &Enum) -> anyhow::Result<Layout> {
    if enum_.variant_values().is_empty() {
        return Ok(Layout::bytes(enum_.byte_size().into()));
    }
    Ok(match enum_.byte_size() {
        1 => Layout::of::<u8>(),
        2 => Layout::of::<u16>(),
        4 => Layout::of::<u32>(),
        8 => Layout::of::<u64>(),
        other => bail!("unsupported enum size: {other}"),
    })
}

#[derive(Debug, Clone, Copy)]
pub(super) enum RustType<'a> {
    Named(&'static str),
    Rtti(&'a Type),
    Bytes(u32),
}

impl<'t> RustType<'t> {
    pub(super) fn display<'d, 'r>(self, dumper: &'d Dumper<'r>) -> RustTypeDisplay<'t, 'd, 'r> {
        RustTypeDisplay { typ: self, dumper }
    }
}

#[derive(Debug)]
pub(super) struct RustTypeDisplay<'t, 'd, 'r> {
    typ: RustType<'t>,
    dumper: &'d Dumper<'r>,
}

impl fmt::Display for RustTypeDisplay<'_, '_, '_> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.typ {
            RustType::Named(name) => out.write_str(name),
            RustType::Bytes(size) => write!(out, "[u8; {size:#X}]"),
            // Plans validate spelling before emission so unsupported RTTI types
            // retain their contextual anyhow error rather than a formatting error.
            RustType::Rtti(typ) => self
                .dumper
                .write_type_name(typ, out)
                .map_err(|_| fmt::Error),
        }
    }
}

#[derive(Debug)]
struct TypeNameSink;

impl fmt::Write for TypeNameSink {
    fn write_str(&mut self, _: &str) -> fmt::Result {
        Ok(())
    }
}

#[derive(Debug)]
pub(super) struct ResolvedType<'a> {
    pub(super) rust_type: RustType<'a>,
    pub(super) layout: Layout,
}

impl ResolvedType<'_> {
    pub(super) fn at_offset(self, offset: u32, is_enum: bool) -> Self {
        if is_enum && !offset.is_multiple_of(self.layout.alignment) {
            // Packed enum properties must use byte alignment for both emission
            // and padding, not just for their printed type.
            Self {
                rust_type: RustType::Bytes(self.layout.size),
                layout: Layout::bytes(self.layout.size),
            }
        } else {
            self
        }
    }
}

#[derive(Debug)]
pub(super) struct ClassPlan<'a> {
    pub(super) fields: Vec<FieldPlan<'a>>,
    pub(super) tail_padding: Option<Padding>,
    pub(super) layout: Layout,

    pub(super) known_base: Option<KnownBase>,
}

#[derive(Debug)]
pub(super) struct FieldPlan<'a> {
    pub(super) name: String,
    pub(super) rust_type: RustType<'a>,
    pub(super) offset: u32,
    pub(super) padding: Option<Padding>,
}

#[derive(Debug)]
pub(super) struct Padding {
    pub(super) name: String,
    size: u32,
}

impl Padding {
    pub(super) fn new(size: u32, count: &mut usize, names: &mut HashSet<String>) -> Option<Self> {
        if size == 0 {
            return None;
        }
        let name = unique_name(format!("_padding{count}"), names);
        *count += 1;
        Some(Self { name, size })
    }

    pub(super) fn write<W: io::Write>(&self, out: &mut W) -> io::Result<()> {
        writeln!(out, "    pub {}: [u8; {:#X}],", self.name, self.size)
    }
}

pub(super) fn unique_name(name: String, names: &mut HashSet<String>) -> String {
    if names.insert(name.clone()) {
        return name;
    }
    for suffix in 1.. {
        let candidate = format!("{name}{suffix}");
        if names.insert(candidate.clone()) {
            return candidate;
        }
    }
    unreachable!()
}

#[derive(Debug, Clone, Copy)]
pub(super) enum KnownBase {
    ISerializable,
    IScriptable,
}
