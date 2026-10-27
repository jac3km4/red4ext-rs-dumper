use std::collections::{BTreeMap, HashMap, HashSet};
use std::{fmt, io, ptr};

use anyhow::{anyhow, bail, ensure};
use heck::{ToPascalCase, ToSnakeCase};
use red4ext_rs::types::{Bitfield, CName, Class, Enum, Property, TaggedType, Type, TypeKind};
use red4ext_rs::RttiSystem;
use resolver::{fixed_array_len, ClassPlan, KnownBase, LayoutResolver};

use crate::layout::Layout;

mod constants;
mod resolver;

#[cfg(test)]
mod tests;

pub struct Dumper<'a> {
    rtti: &'a RttiSystem,
    known_types: HashMap<*const Type, KnownType>,
    rust_type_names: BTreeMap<CName, String>,
    class_properties: HashMap<CName, Vec<&'a Property>>,
    alignment_overrides: HashMap<CName, u32>,
    derive_whitelist: HashSet<CName>,
    excluded_classes: HashSet<CName>,
}

impl<'a> Dumper<'a> {
    pub fn new(rtti: &'a RttiSystem) -> Self {
        let script_alias_blacklist: HashSet<_> = constants::SCRIPT_ALIAS_BLACKLIST
            .iter()
            .copied()
            .map(CName::new)
            .collect();
        let excluded_classes: HashSet<_> = constants::EXCLUDED_CLASSES
            .iter()
            .copied()
            .chain(constants::KNOWN_TYPE_MAPPING.iter().map(|&(name, _)| name))
            .map(CName::new)
            .collect();

        let native_to_script = rtti.native_to_script_map();
        let mut property_collector = PropertyCollector::default();
        let mut rust_type_names = BTreeMap::new();

        for (_, typ) in rtti.type_map() {
            if let Some(name) = match typ.tagged() {
                TaggedType::Class(class) => {
                    if !excluded_classes.contains(&class.name()) {
                        property_collector.process(class);
                    }
                    Some(class.name())
                }
                TaggedType::Enum(enum_) => Some(enum_.name()),
                TaggedType::BitField(bf) => Some(bf.name()),
                _ => None,
            } {
                let rust_name = native_to_script
                    .get(&name)
                    .filter(|n| {
                        rtti.type_map().get(n).is_none() && !script_alias_blacklist.contains(n)
                    })
                    .copied()
                    .unwrap_or(name)
                    .as_str()
                    .to_pascal_case();
                rust_type_names.insert(name, rust_name);
            }
        }

        Self {
            rtti,
            known_types: constants::KNOWN_TYPE_MAPPING
                .iter()
                .filter_map(|&(name, mapped)| {
                    rtti.get_type(CName::new(name))
                        .map(|typ| (ptr::from_ref(typ), mapped))
                })
                .collect(),
            rust_type_names,
            class_properties: property_collector.classes,
            alignment_overrides: constants::ALIGNMENT_OVERRIDES
                .iter()
                .map(|&(name, align)| (CName::new(name), align))
                .collect(),
            derive_whitelist: constants::DERIVE_WHITELIST
                .iter()
                .copied()
                .map(CName::new)
                .collect(),
            excluded_classes,
        }
    }

    pub fn write<W: io::Write>(&self, out: &mut W) -> anyhow::Result<()> {
        let mut layouts = LayoutResolver::new(self);
        layouts.validate_all()?;
        writeln!(out, "use red4ext_rs::types::*;")?;
        writeln!(
            out,
            "use red4ext_rs::{{class_kind, NativeRepr, ScriptClass}};"
        )?;
        writeln!(out)?;

        for name in self.rust_type_names.keys() {
            let typ = self
                .rtti
                .get_type(*name)
                .ok_or_else(|| anyhow!("could not resolve type {name}"))?;

            match typ.tagged() {
                TaggedType::Class(class) => {
                    if self.excluded_classes.contains(&class.name()) {
                        continue;
                    }
                    let plan = layouts.class_plan(class)?;
                    self.write_class(out, class, plan)?;
                }
                TaggedType::Enum(enum_) => {
                    self.write_enum(out, enum_)?;
                }
                TaggedType::BitField(bf) => {
                    self.write_bitfield(out, bf)?;
                }
                _ => {}
            }
        }

        writeln!(out, "#[cfg(test)]")?;
        writeln!(out, "mod tests {{")?;
        writeln!(out, "    use super::*;")?;
        writeln!(out)?;
        for name in self.rust_type_names.keys() {
            if self.excluded_classes.contains(name) {
                continue;
            }
            if let Some(class) = self.rtti.get_class(*name) {
                self.write_test(out, class, layouts.class_plan(class)?)?;
            }
        }
        writeln!(out, "}}")?;

        Ok(())
    }

    fn write_class<W: io::Write>(
        &self,
        out: &mut W,
        class: &Class,
        plan: &ClassPlan<'_>,
    ) -> anyhow::Result<()> {
        let mapped_name = self.rust_type_name(class.name())?;

        const WHITELIST_DERIVES: &[&str] = &["Debug", "PartialEq", "PartialOrd"];
        const SMALL_TYPE_DERIVES: &[&str] = &["Clone", "Copy"];

        let is_copy_eligible = class.size() <= 16
            && class
                .properties()
                .iter()
                .all(|p| matches!(p.type_().kind(), TypeKind::Name | TypeKind::Fundamental));

        let mut derives = self
            .derive_whitelist
            .contains(&class.name())
            .then(|| WHITELIST_DERIVES.iter())
            .into_iter()
            .chain(is_copy_eligible.then(|| SMALL_TYPE_DERIVES.iter()))
            .flatten()
            .peekable();

        if derives.peek().is_some() {
            write!(out, "#[derive(")?;
            for derive in derives {
                write!(out, "{}, ", derive)?;
            }
            writeln!(out, ")]")?;
        }

        let alignment = plan.layout.alignment;
        if alignment > 1 {
            writeln!(out, "#[repr(C, align({alignment}))]")?;
        } else {
            writeln!(out, "#[repr(C)]")?;
        }
        writeln!(out, "pub struct {mapped_name} {{")?;

        for field in &plan.fields {
            if let Some(padding) = &field.padding {
                padding.write(out)?;
            }
            writeln!(
                out,
                "    pub {}: {}, // {:#X}",
                field.name,
                field.rust_type.display(self),
                field.offset
            )?;
        }
        if let Some(padding) = &plan.tail_padding {
            padding.write(out)?;
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        if class.is_class() {
            writeln!(out, "unsafe impl ScriptClass for {mapped_name} {{",)?;
            writeln!(
                out,
                "    const NAME: &'static str = \"{}\";",
                class.name().as_str()
            )?;
            if class.flags().is_native() {
                writeln!(out, "    type Kind = class_kind::Native;",)?;
            } else {
                writeln!(out, "    type Kind = class_kind::Scripted;",)?;
            }
            writeln!(out, "}}")?;
        } else {
            writeln!(out, "unsafe impl NativeRepr for {mapped_name} {{",)?;
            writeln!(
                out,
                "    const NAME: &'static str = \"{}\";",
                class.name().as_str()
            )?;
            writeln!(out, "}}")?;
        }
        writeln!(out)?;

        match plan.known_base {
            Some(KnownBase::ISerializable) => {
                writeln!(out, "impl AsRef<ISerializable> for {mapped_name} {{")?;
                writeln!(out, "    #[inline]")?;
                writeln!(out, "    fn as_ref(&self) -> &ISerializable {{")?;
                writeln!(out, "        &self.base")?;
                writeln!(out, "    }}")?;
                writeln!(out, "}}")?;
                writeln!(out)?;
            }
            Some(KnownBase::IScriptable) => {
                writeln!(out, "impl AsRef<IScriptable> for {mapped_name} {{")?;
                writeln!(out, "    #[inline]")?;
                writeln!(out, "    fn as_ref(&self) -> &IScriptable {{")?;
                writeln!(out, "        &self.base")?;
                writeln!(out, "    }}")?;
                writeln!(out, "}}")?;
                writeln!(out)?;
            }
            None => {}
        }

        Ok(())
    }

    fn write_enum<W: io::Write>(&self, out: &mut W, enum_: &Enum) -> anyhow::Result<()> {
        if enum_.variant_values().is_empty() {
            writeln!(out, "#[derive(Debug, Clone, Copy)]")?;
            writeln!(out, "#[repr(transparent)]")?;
            writeln!(
                out,
                "pub struct {}(pub [u8; {:#X}]);",
                self.rust_type_name(enum_.name())?,
                enum_.byte_size()
            )?;
            return Ok(());
        }

        let is_unsigned = enum_.variant_values().iter().all(|&v| v >= 0);
        let typ = match enum_.byte_size() {
            1 if is_unsigned => "u8",
            1 => "i8",
            2 if is_unsigned => "u16",
            2 => "i16",
            4 if is_unsigned => "u32",
            4 => "i32",
            8 if is_unsigned => "u64",
            8 => "i64",
            other => bail!("unsupported enum size: {other}"),
        };
        writeln!(
            out,
            "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]"
        )?;
        writeln!(out, "#[repr({typ})]")?;
        writeln!(out, "pub enum {} {{", self.rust_type_name(enum_.name())?)?;
        let mut variants = enum_
            .variant_names()
            .iter()
            .zip(enum_.variant_values())
            .collect::<Vec<_>>();
        variants.sort_by_key(|(_, val)| *val);
        variants.dedup_by_key(|(_, val)| *val);

        for (name, val) in variants {
            writeln!(
                out,
                "    {} = {},",
                sanitize_enum_variant(name.as_str()),
                val
            )?;
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(
            out,
            "unsafe impl NativeRepr for {} {{",
            self.rust_type_name(enum_.name())?
        )?;
        writeln!(
            out,
            "    const NAME: &'static str = \"{}\";",
            enum_.name().as_str()
        )?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        Ok(())
    }

    fn write_bitfield<W: io::Write>(&self, out: &mut W, bitfield: &Bitfield) -> anyhow::Result<()> {
        writeln!(out, "#[derive(Debug, Clone, Copy)]")?;
        writeln!(out, "#[repr(transparent)]")?;
        writeln!(
            out,
            "pub struct {}(pub [u8; {:#X}]);",
            self.rust_type_name(bitfield.name())?,
            bitfield.byte_size()
        )?;
        writeln!(out)?;

        Ok(())
    }

    fn write_test<W: io::Write>(
        &self,
        out: &mut W,
        class: &Class,
        plan: &ClassPlan<'_>,
    ) -> anyhow::Result<()> {
        let name = self.rust_type_name(class.name())?;
        writeln!(out, "    #[test]")?;
        writeln!(
            out,
            "    fn test_{}_layout() {{",
            class.name().as_str().to_snake_case()
        )?;
        writeln!(
            out,
            "        assert_eq!(std::mem::size_of::<{name}>(), {});",
            plan.layout.size
        )?;
        writeln!(
            out,
            "        assert_eq!(std::mem::align_of::<{name}>(), {});",
            plan.layout.alignment
        )?;
        for field in &plan.fields {
            writeln!(
                out,
                "        assert_eq!(std::mem::offset_of!({name}, {}), {:#X});",
                field.name, field.offset
            )?;
        }
        writeln!(out, "    }}")?;
        Ok(())
    }

    fn rust_type_name(&self, name: CName) -> anyhow::Result<&str> {
        self.rust_type_names
            .get(&name)
            .map(AsRef::as_ref)
            .ok_or_else(|| anyhow!("could not resolve type {}", name.as_str()))
    }

    // Type spelling is independent of layout resolution: handles and arrays may
    // legally refer back to the class currently being generated.
    fn write_type_name(&self, typ: &Type, out: &mut impl fmt::Write) -> anyhow::Result<()> {
        if let Some(mapped) = self.known_types.get(&ptr::from_ref(typ)) {
            out.write_str(mapped.rust_name)?;
            return Ok(());
        }

        let (prefix, inner, suffix, count) = match typ.tagged() {
            TaggedType::Name => {
                out.write_str("CName")?;
                return Ok(());
            }
            TaggedType::Class(class) => {
                ensure!(
                    !self.excluded_classes.contains(&class.name()),
                    "no Rust representation for excluded class {}",
                    class.name()
                );
                out.write_str(self.rust_type_name(class.name())?)?;
                return Ok(());
            }
            TaggedType::Enum(enum_) => {
                out.write_str(self.rust_type_name(enum_.name())?)?;
                return Ok(());
            }
            TaggedType::BitField(bf) => {
                out.write_str(self.rust_type_name(bf.name())?)?;
                return Ok(());
            }
            TaggedType::Array(array) => ("RedArray<", array.element_type(), ">", None),
            TaggedType::StaticArray(array) => (
                "StaticArray<",
                array.element_type(),
                ">",
                Some((", ", array.size())),
            ),
            TaggedType::NativeArray(array) => {
                ("[", array.element_type(), "]", Some(("; ", array.size())))
            }
            TaggedType::FixedArray(array) => (
                "[",
                array.element_type(),
                "]",
                Some(("; ", fixed_array_len(typ)?)),
            ),
            TaggedType::Ref(ref_) => ("Ref<", ref_.pointee(), ">", None),
            TaggedType::WeakRef(ref_) => ("WeakRef<", ref_.pointee(), ">", None),
            TaggedType::ResourceRef(ref_) => ("ResourceRef<", ref_.resource_type(), ">", None),
            TaggedType::RaRef(ref_) => ("RaRef<", ref_.resource_type(), ">", None),
            TaggedType::Curve(curve) => ("Curve<", curve.element_type(), ">", None),
            _ => bail!("unsupported RTTI type {} ({:?})", typ.name(), typ.kind()),
        };
        out.write_str(prefix)?;
        self.write_type_name(inner, out)?;
        if let Some((separator, count)) = count {
            write!(out, "{separator}{count}")?;
        }
        out.write_str(suffix)?;
        Ok(())
    }
}

impl fmt::Debug for Dumper<'_> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        out.debug_struct("Dumper")
            .field("rtti", &ptr::from_ref(self.rtti))
            .field("known_types", &self.known_types.len())
            .field("rust_type_names", &self.rust_type_names.len())
            .field("class_properties", &self.class_properties.len())
            .field("alignment_overrides", &self.alignment_overrides.len())
            .field("derive_whitelist", &self.derive_whitelist.len())
            .field("excluded_classes", &self.excluded_classes.len())
            .finish()
    }
}

impl fmt::Display for Dumper<'_> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(out, "report:")?;
        writeln!(out, "Rust type names    : {}", self.rust_type_names.len())?;
        writeln!(out, "class properties   : {}", self.class_properties.len())?;
        writeln!(
            out,
            "alignment overrides: {}",
            self.alignment_overrides.len()
        )?;
        writeln!(out, "derive whitelist   : {}", self.derive_whitelist.len())?;
        write!(out, "excluded classes   : {}", self.excluded_classes.len())
    }
}

#[derive(Debug, Default)]
struct PropertyCollector<'a> {
    classes: HashMap<CName, Vec<&'a Property>>,
}

impl<'a> PropertyCollector<'a> {
    fn process(&mut self, class: &'a Class) {
        let mut props = self.classes.remove(&class.name()).unwrap_or_default();
        let eligible_props = class
            .properties()
            .iter()
            .filter(|p| p.flags().in_value_holder() != class.flags().is_native());
        props.extend(eligible_props);
        props.sort_by_key(|prop| prop.value_offset());
        props.dedup_by_key(|prop| prop.value_offset());

        // the RTTI defines properties for parents within base classes in some cases,
        // so we need to go down the inheritance chain and assign properties to their respective
        // classes
        if let Some(base) = class.base() {
            let parent_props = props
                .iter()
                .filter(|p| !p.flags().in_value_holder())
                .take_while(|p| p.value_offset() < base.size())
                .count();
            let parent_props = props.drain(..parent_props);

            let mut current = base;
            for prop in parent_props.rev() {
                while let Some(owner) = current.base().filter(|c| prop.value_offset() < c.size()) {
                    current = owner;
                }

                let owner_props = self.classes.entry(current.name()).or_default();
                if let Err(e) =
                    owner_props.binary_search_by(|p| p.value_offset().cmp(&prop.value_offset()))
                {
                    owner_props.insert(e, prop);
                }
            }
        }

        self.classes.insert(class.name(), props);
    }
}

fn sanitize_field(name: &str, i: usize) -> String {
    let str = name.to_snake_case();

    // rename fields that conflict with Rust keywords
    match &str[..] {
        "" => format!("_{}", i),
        "box" => "box_".to_string(),
        "enum" => "enum_".to_string(),
        "false" => "false_".to_string(),
        "impl" => "impl_".to_string(),
        "loop" => "loop_".to_string(),
        "move" => "move_".to_string(),
        "override" => "override_".to_string(),
        "ref" => "ref_".to_string(),
        "self" => "self_".to_string(),
        "struct" => "struct_".to_string(),
        "trait" => "trait_".to_string(),
        "true" => "true_".to_string(),
        "type" => "type_".to_string(),
        "use" => "use_".to_string(),
        _ => str,
    }
}

fn sanitize_enum_variant(name: &str) -> String {
    let str = name.to_pascal_case();

    // rename variants that conflict with Rust keywords
    match &str[..] {
        "" => "_".to_string(),
        "Self" => "Self_".to_string(),
        _ if str.chars().next().is_some_and(|c| c.is_ascii_digit()) => format!("_{}", str),
        _ => str,
    }
}

#[derive(Debug, Clone, Copy)]
struct KnownType {
    rust_name: &'static str,
    layout: Layout,
}

impl KnownType {
    const fn new<T>(rust_name: &'static str) -> Self {
        Self {
            rust_name,
            layout: Layout::of::<T>(),
        }
    }
}
