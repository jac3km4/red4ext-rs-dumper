use std::collections::{HashMap, HashSet};
use std::{fmt, io, iter, mem, ptr};

use anyhow::{anyhow, bail};
use heck::{ToPascalCase, ToSnakeCase};
use red4ext_rs::types::{Bitfield, CName, Class, Enum, Kind, Property, TaggedType, Type, ValuePtr};
use red4ext_rs::{log, RttiSystem};

pub struct Dumper<'a> {
    rtti: &'a RttiSystem,
    fundamental_types: HashMap<*const Type, &'static str>,
    formatted_names: HashMap<CName, String>,
    class_props: HashMap<CName, Vec<&'a Property>>,
    alignment_overrides: HashMap<CName, u32>,
    derive_whitelist: HashSet<CName>,
    native_class_blacklist: HashSet<CName>,
}

impl<'a> Dumper<'a> {
    pub fn new(rtti: &'a RttiSystem) -> anyhow::Result<Self> {
        let script_alias_blacklist: HashSet<_> = constants::SCRIPT_ALIAS_BLACKLIST
            .iter()
            .copied()
            .map(CName::new)
            .collect();
        let native_class_blacklist: HashSet<_> = constants::NATIVE_CLASS_BLACKLIST
            .iter()
            .copied()
            .map(CName::new)
            .collect();

        let native_to_script = rtti.native_to_script_map();
        let mut prop_collector = PropCollector::default();
        let mut formatted_type_names = HashMap::new();

        for (_, typ) in rtti.types() {
            if let Some(name) = match typ.tagged() {
                TaggedType::Class(class) => {
                    if !native_class_blacklist.contains(&class.name()) {
                        prop_collector.process(class);
                    }
                    Some(class.name())
                }
                TaggedType::Enum(enum_) => Some(enum_.name()),
                TaggedType::BitField(bf) => Some(bf.name()),
                _ => None,
            } {
                let pretty_name = native_to_script
                    .get(&name)
                    .filter(|n| {
                        rtti.types().get(n).is_none() && !script_alias_blacklist.contains(n)
                    })
                    .copied()
                    .unwrap_or(name)
                    .as_str()
                    .to_pascal_case();
                formatted_type_names.insert(name, pretty_name);
            }
        }

        Ok(Self {
            rtti,
            fundamental_types: constants::FUNDAMENTAL_TYPE_MAPPING
                .iter()
                .map(|&(from, to)| {
                    let ty = rtti
                        .get_type(CName::new(from))
                        .ok_or_else(|| anyhow!("could not resolve {from} type"))?;
                    Ok((ptr::from_ref(ty), to))
                })
                .collect::<anyhow::Result<_>>()?,
            formatted_names: formatted_type_names,
            class_props: prop_collector.classes,
            alignment_overrides: constants::ALIGNMENT_OVERRIDES
                .iter()
                .map(|&(name, align)| (CName::new(name), align))
                .collect(),
            derive_whitelist: constants::DERIVE_WHITELIST
                .iter()
                .copied()
                .map(CName::new)
                .collect(),
            native_class_blacklist,
        })
    }

    pub fn write<W: io::Write>(&self, out: &mut W) -> anyhow::Result<()> {
        writeln!(out, "use red4ext_rs::NativeRepr;")?;
        writeln!(out, "use red4ext_rs::types::*;")?;
        writeln!(out)?;

        for name in self.formatted_names.keys() {
            let typ = self
                .rtti
                .get_type(*name)
                .ok_or_else(|| anyhow!("could not resolve type {name}"))?;

            match typ.tagged() {
                TaggedType::Class(class) => {
                    if self.native_class_blacklist.contains(&class.name()) {
                        continue;
                    }
                    self.write_class(out, class)?;
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
        for name in self.class_props.keys() {
            let class = self
                .rtti
                .get_class(*name)
                .ok_or_else(|| anyhow!("could not resolve class {name}"))?;
            self.write_test(out, class)?;
        }
        writeln!(out, "}}")?;

        Ok(())
    }

    fn write_class<W: io::Write>(&self, out: &mut W, class: &Class) -> anyhow::Result<()> {
        let mut padding_fields = 0;
        let mut current_size = 0;
        let mut names_seen: HashMap<String, usize> = HashMap::new();
        let mapped_name = self.map_name(class.name())?;

        let alignment_override = self.alignment_overrides.get(&class.name()).copied();
        let alignment = alignment_override.unwrap_or(class.alignment());

        const WHITELIST_DERIVES: &[&str] = &["Debug", "PartialEq", "PartialOrd"];
        const SMALL_TYPE_DERIVES: &[&str] = &["Clone", "Copy"];

        let is_copy_eligible = class.size() <= 16
            && class
                .properties()
                .iter()
                .all(|p| matches!(p.type_().kind(), Kind::Name | Kind::Fundamental));

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

        if !class.is_class() {
            writeln!(out, "#[repr(C, align({}))]", alignment)?;
        } else if let Some(alignment) = alignment_override {
            writeln!(out, "#[repr(C, align({}))]", alignment)?;
        } else {
            writeln!(out, "#[repr(C)]")?;
        };
        writeln!(out, "pub struct {mapped_name} {{")?;

        let mut bases = iter::once(class)
            .chain(class.base_iter())
            .take_while(|c| c.flags().is_native() == class.flags().is_native())
            .collect::<Vec<_>>();
        bases.reverse();

        let (bases, known_base) = match &bases[..] {
            [_, base, rem @ ..] if base.name() == CName::new("IScriptable") => {
                writeln!(out, "    pub base: IScriptable,")?;
                current_size = base.size();
                (rem, Some(KnownBase::IScriptable))
            }
            [base, rem @ ..] if base.name() == CName::new("ISerializable") => {
                writeln!(out, "    pub base: ISerializable,")?;
                current_size = base.size();
                (rem, Some(KnownBase::ISerializable))
            }
            [base, rem @ ..] if base.name() == CName::new("gameScriptableSystem") => {
                writeln!(out, "    pub base: ScriptableSystem,")?;
                current_size = base.size();
                (rem, Some(KnownBase::ScriptableSystem))
            }
            _ => (&bases[..], None),
        };

        if known_base.is_some() {
            names_seen.insert("base".into(), 0);
        }

        for (i, prop) in bases
            .iter()
            .flat_map(|base| {
                self.class_props
                    .get(&base.name())
                    .map(Vec::as_slice)
                    .unwrap_or_default()
            })
            .enumerate()
        {
            let offset = prop.value_offset();
            let Some(padding) = offset.checked_sub(current_size) else {
                log::warn!(
                    "Skipping property {} in class {}",
                    prop.name().as_str(),
                    class.name().as_str(),
                );
                continue;
            };

            let field_alignment = prop
                .type_()
                .as_class()
                .and_then(|c| self.alignment_overrides.get(&c.name()))
                .copied()
                .unwrap_or(prop.type_().alignment());
            if offset != current_size.next_multiple_of(field_alignment) {
                if padding > 0 {
                    writeln!(
                        out,
                        "    pub _padding{}: [u8; {:#X}],",
                        padding_fields, padding
                    )?;

                    padding_fields += 1;
                } else {
                    log::warn!(
                        "Non-aligned property {} in class {}",
                        prop.name().as_str(),
                        class.name().as_str()
                    );
                }
            }
            let sane_name = santize_field(prop.name().as_str(), i);
            let i = names_seen
                .entry(sane_name.clone())
                .and_modify(|x| *x += 1)
                .or_default();

            write!(out, "    pub {sane_name}")?;
            if *i > 0 {
                write!(out, "{i}")?;
            }
            match prop.type_().tagged() {
                TaggedType::Enum(e) if offset != offset.next_multiple_of(e.byte_size().into()) => {
                    // there are unaligned enums in the RTTI for some reason
                    write!(out, ": [u8; {:#X}]", e.byte_size())?;
                }
                _ => {
                    let typ = TypeFormatter::new(
                        prop.type_(),
                        &self.fundamental_types,
                        &self.formatted_names,
                    );
                    write!(out, ": {typ}")?;
                }
            }
            writeln!(out, ", // {:#X}", offset)?;

            current_size = offset + prop.type_().size();
        }

        match class.properties_size().checked_sub(current_size) {
            None => {
                log::warn!("Properties of {} exceed its size", class.name().as_str());
            }
            Some(i) if i >= alignment || (current_size == 0 && i > 0) => {
                writeln!(out, "    pub _padding{}: [u8; {:#X}],", padding_fields, i)?;
            }
            _ => {}
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        if class.is_class() {
            writeln!(out, "unsafe impl ScriptClass for {mapped_name} {{",)?;
            writeln!(
                out,
                "    const CLASS_NAME: &'static str = \"{}\";",
                class.name().as_str()
            )?;
            if class.flags().is_native() {
                writeln!(out, "    type Kind = Native;",)?;
            } else {
                writeln!(out, "    type Kind = Scripted;",)?;
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

        match known_base {
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
            Some(KnownBase::ScriptableSystem) => {
                writeln!(out, "impl AsRef<ScriptableSystem> for {mapped_name} {{")?;
                writeln!(out, "    #[inline]")?;
                writeln!(out, "    fn as_ref(&self) -> &ScriptableSystem {{")?;
                writeln!(out, "        &self.base")?;
                writeln!(out, "    }}")?;
                writeln!(out, "}}")?;
                writeln!(out)?;
                writeln!(out, "impl AsRef<IScriptable> for {mapped_name} {{")?;
                writeln!(out, "    #[inline]")?;
                writeln!(out, "    fn as_ref(&self) -> &IScriptable {{")?;
                writeln!(out, "        &self.base.as_ref()")?;
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
            writeln!(
                out,
                "pub struct {}(pub [u8; {:#X}]);",
                self.map_name(enum_.name())?,
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
        writeln!(out, "pub enum {} {{", self.map_name(enum_.name())?)?;
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
            self.map_name(enum_.name())?
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
            self.map_name(bitfield.name())?,
            bitfield.byte_size()
        )?;
        writeln!(out)?;

        Ok(())
    }

    fn write_test<W: io::Write>(&self, out: &mut W, class: &Class) -> anyhow::Result<()> {
        writeln!(out, "    #[test]")?;
        writeln!(
            out,
            "    fn test_{}_size() {{",
            class.name().as_str().to_snake_case()
        )?;
        writeln!(
            out,
            "        assert_eq!(std::mem::size_of::<{}>().next_multiple_of(16), {});",
            self.map_name(class.name())?,
            class.properties_size().next_multiple_of(16)
        )?;
        writeln!(out, "    }}")?;
        Ok(())
    }

    fn map_name(&self, name: CName) -> anyhow::Result<&str> {
        self.formatted_names
            .get(&name)
            .map(AsRef::as_ref)
            .ok_or_else(|| anyhow!("could not resolve type {}", name.as_str()))
    }
}

#[derive(Debug, Default)]
struct PropCollector<'a> {
    classes: HashMap<CName, Vec<&'a Property>>,
}

impl<'a> PropCollector<'a> {
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

fn santize_field(name: &str, i: usize) -> String {
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

#[derive(Debug)]
struct TypeFormatter<'a> {
    typ: &'a Type,
    fundamental_types: &'a HashMap<*const Type, &'static str>,
    formatted_names: &'a HashMap<CName, String>,
}

impl<'a> TypeFormatter<'a> {
    fn new(
        typ: &'a Type,
        fundamentals: &'a HashMap<*const Type, &'static str>,
        type_names: &'a HashMap<CName, String>,
    ) -> Self {
        Self {
            typ,
            fundamental_types: fundamentals,
            formatted_names: type_names,
        }
    }

    fn format(&self, typ: &'a Type) -> Self {
        Self::new(typ, self.fundamental_types, self.formatted_names)
    }
}

impl<'a> fmt::Display for TypeFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.typ.tagged() {
            TaggedType::Name => write!(f, "CName"),
            TaggedType::Fundamental | TaggedType::Simple => {
                if let Some(name) = self.fundamental_types.get(&ptr::from_ref(self.typ)) {
                    write!(f, "{}", name)
                } else {
                    write!(f, "!")
                }
            }
            TaggedType::Class(class) => {
                write!(f, "{}", self.formatted_names.get(&class.name()).unwrap())
            }
            TaggedType::Array(array) => write!(f, "RedArray<{}>", self.format(array.inner_type())),
            TaggedType::Enum(enum_) => {
                write!(f, "{}", self.formatted_names.get(&enum_.name()).unwrap())
            }
            TaggedType::StaticArray(array) => write!(
                f,
                "StaticArray<{}, {}>",
                self.format(array.element_type()),
                array.size()
            ),
            TaggedType::NativeArray(array) => {
                write!(
                    f,
                    "[{}; {}]",
                    self.format(array.element_type()),
                    array.size()
                )
            }
            TaggedType::Ref(ref_) => {
                write!(f, "Ref<{}>", self.format(ref_.pointee()))
            }
            TaggedType::WeakRef(wref) => write!(f, "WeakRef<{}>", self.format(wref.pointee())),
            TaggedType::ResourceRef(ref_) => {
                write!(f, "ResourceRef<{}>", self.format(ref_.resource_type()))
            }
            TaggedType::RaRef(ref_) => write!(f, "RaRef<{}>", self.format(ref_.resource_type())),
            TaggedType::BitField(bf) => {
                write!(f, "{}", self.formatted_names.get(&bf.name()).unwrap())
            }
            TaggedType::Curve(typ) => write!(f, "Curve<{}>", self.format(typ.element_type())),
            TaggedType::FixedArray(array) => {
                write!(f, "[{}; {}]", self.format(array.inner_type()), unsafe {
                    array.length(mem::transmute::<*mut (), ValuePtr>(ptr::null_mut()))
                },)
            }
            _ => unimplemented!(),
        }
    }
}

enum KnownBase {
    ISerializable,
    IScriptable,
    ScriptableSystem,
}

mod constants {
    pub(super) const FUNDAMENTAL_TYPE_MAPPING: &[(&str, &str)] = &[
        ("Bool", "bool"),
        ("Int8", "i8"),
        ("Uint8", "u8"),
        ("Int16", "i16"),
        ("Uint16", "u16"),
        ("Int32", "i32"),
        ("Uint32", "u32"),
        ("Int64", "i64"),
        ("Uint64", "u64"),
        ("Float", "f32"),
        ("Double", "f64"),
        ("String", "RedString"),
        ("LocalizationString", "LocalizationString"),
        ("CName", "CName"),
        ("TweakDBID", "TweakDbId"),
        ("NodeRef", "NodeRef"),
        ("DataBuffer", "DataBuffer"),
        ("serializationDeferredDataBuffer", "DeferredDataBuffer"),
        ("SharedDataBuffer", "SharedDataBuffer"),
        ("CDateTime", "DateTime"),
        ("CGUID", "Guid"),
        ("CRUID", "Cruid"),
        ("EditorObjectID", "EditorObjectId"),
        ("MessageResourcePath", "MessageResourcePath"),
        ("Variant", "Variant"),
        ("multiChannelCurve:Float", "MultiChannelCurve<f32>"),
    ];

    pub(super) const NATIVE_CLASS_BLACKLIST: &[&str] = &[
        // these types are not useable due to not being defined correctly in the RTTI
        "scnChatterModuleSharedState",
        "gameuiUIGameState",
        // these types have hand-written implementations
        "ISerializable",
        "IScriptable",
        "redResourceReferenceScriptToken",
        "ResourceRef",
        "ResourceAsyncRef",
        "EngineTime",
        "GameTime",
        "entEntityID",
        "gameItemID",
        "ScriptGameInstance",
        "gameScriptableSystem",
    ];

    // these script aliases cause conflicts with existing types
    pub(super) const SCRIPT_ALIAS_BLACKLIST: &[&str] = &["FTResult"];

    // these classes that should derive Debug, PartialEq and PartialOrd,
    // we need to be selective because deriving too many traits causes
    // a massive increase in compile times
    pub(super) const DERIVE_WHITELIST: &[&str] = &[
        "Vector2",
        "Vector3",
        "Vector4",
        "Quaternion",
        "EulerAngles",
        "Matrix",
        "Transform",
        "FixedPoint",
        "WorldPosition",
        "Color",
        "HDRColor",
        "Rect",
        "RectF",
        "Plane",
        "Box",
        "Sphere",
        "Cylinder",
        "Point",
    ];

    // these types have alignment issues in the RTTI
    pub(super) const ALIGNMENT_OVERRIDES: &[(&str, u32)] = &[
        ("Color", 1),
        ("PSODescStencilFuncDesc", 1),
        ("PSODescRenderTarget", 1),
        ("GpuWrapApiVertexPackingPackingElement", 1),
        ("entRenderToTextureFeatures", 1),
        ("scneventsCameraOverrideSettings", 1),
        ("worldProxyCustomGeometryParams", 1),
        ("netPeerID", 2),
        ("NavGenNavigationSetting", 2),
        ("vehicleVehicleSlotsState", 4),
    ];
}
