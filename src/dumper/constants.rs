use red4ext_rs::types::*;

use super::KnownType;

macro_rules! known {
    ($typ:ty) => {
        KnownType::new::<$typ>(stringify!($typ))
    };
}

pub(super) const KNOWN_TYPE_MAPPING: &[(&str, KnownType)] = &[
    ("Bool", known!(bool)),
    ("Int8", known!(i8)),
    ("Uint8", known!(u8)),
    ("Int16", known!(i16)),
    ("Uint16", known!(u16)),
    ("Int32", known!(i32)),
    ("Uint32", known!(u32)),
    ("Int64", known!(i64)),
    ("Uint64", known!(u64)),
    ("Float", known!(f32)),
    ("Double", known!(f64)),
    ("String", known!(RedString)),
    ("LocalizationString", known!(LocalizationString)),
    ("CName", known!(CName)),
    ("TweakDBID", known!(TweakDbId)),
    ("NodeRef", known!(NodeRef)),
    ("DataBuffer", known!(DataBuffer)),
    (
        "serializationDeferredDataBuffer",
        known!(DeferredDataBuffer),
    ),
    ("SharedDataBuffer", known!(SharedDataBuffer)),
    ("CDateTime", known!(DateTime)),
    ("CGUID", known!(Guid)),
    ("CRUID", known!(Cruid)),
    ("EditorObjectID", known!(EditorObjectId)),
    ("MessageResourcePath", known!(MessageResourcePath)),
    ("Variant", known!(Variant)),
    ("multiChannelCurve:Float", known!(MultiChannelCurve<f32>)),
    ("ISerializable", known!(ISerializable)),
    ("IScriptable", known!(IScriptable)),
    ("redResourceReferenceScriptToken", known!(ResRef)),
    (
        "ResourceRef",
        KnownType::new::<ResourceRef<u8>>("ResourceRef<u8>"),
    ),
    ("ResourceAsyncRef", KnownType::new::<RaRef<u8>>("RaRef<u8>")),
    ("EngineTime", known!(EngineTime)),
    ("GameTime", known!(GameTime)),
    ("entEntityID", known!(EntityId)),
    ("gameItemID", known!(ItemId)),
    ("ScriptGameInstance", known!(GameInstance)),
    ("gameScriptableSystem", known!(ScriptableSystem)),
];

pub(super) const EXCLUDED_CLASSES: &[&str] = &[
    // these types are not useable due to not being defined correctly in the RTTI
    "scnChatterModuleSharedState",
    "gameuiUIGameState",
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

// Minimum alignment corrections supported by the SDK value layouts. These
// correct RTTI metadata, not field padding; effective Rust alignment also
// includes all member alignments. Only retain corrections that cannot be
// inferred from size compatibility or the natural alignment of members.
pub(super) const ALIGNMENT_OVERRIDES: &[(&str, u32)] = &[
    ("Color", 1),
    ("SOMState", 1),
    ("PSODescDepthStencilModeDesc", 1),
    ("PSODescRasterizerModeDesc", 1),
    ("PSODescStencilFuncDesc", 1),
    ("PSODescRenderTarget", 1),
    ("scneventsCameraOverrideSettings", 1),
    ("worldProxyCustomGeometryParams", 1),
];
