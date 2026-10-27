use red4ext_rs::types::{ISerializable, StaticArray, TweakDbId};

use super::resolver::{
    finish_class_layout, report_layout_errors, unique_name, Padding, ResolvedType, RustType,
};
use super::*;
use crate::layout::{size_compatible_alignment, StructLayout};

#[test]
fn known_metadata_matches_the_emitted_wrapper() {
    let (_, mapped) = constants::KNOWN_TYPE_MAPPING
        .iter()
        .find(|(name, _)| *name == "TweakDBID")
        .unwrap();
    assert_eq!(mapped.rust_name, "TweakDbId");
    assert_eq!(mapped.layout, Layout::of::<TweakDbId>());
    assert_eq!(mapped.layout.alignment, 1);
    for (end, offset) in [
        (0x325, 0x328),
        (0x74E, 0x750),
        (0xDF, 0xE0),
        (0x11A, 0x11C),
        (0x143, 0x144),
        (0x28B, 0x28C),
    ] {
        let mut layout = StructLayout::new(1).unwrap();
        layout.push(0, Layout::bytes(end)).unwrap();
        assert_eq!(layout.push(offset, mapped.layout).unwrap(), offset - end);
    }
}

#[test]
fn inline_and_static_arrays_match_dependency_layouts() {
    let element = Layout::of::<TweakDbId>();
    assert_eq!(element.array(3).unwrap(), Layout::of::<[TweakDbId; 3]>());
    assert_eq!(
        element.static_array(3).unwrap(),
        Layout::of::<StaticArray<TweakDbId, 3>>()
    );
    assert_eq!(
        Layout::of::<u64>().static_array(3).unwrap(),
        Layout::of::<StaticArray<u64, 3>>()
    );
    assert_eq!(
        Layout::bytes(4).array(5).unwrap().array(2).unwrap(),
        Layout::of::<[[u8; 4]; 5 * 2]>()
    );
}

#[test]
fn sdk_color_array_uses_element_alignment_not_enclosing_override() {
    // BloomAreaSettings contains Color[5] at 0x61 but inherits an 8-aligned base.
    let mut layout = StructLayout::new(1).unwrap();
    layout.push(0, Layout::of::<ISerializable>()).unwrap();
    layout.push(0x30, Layout::of::<bool>()).unwrap();
    layout.push(0x38, Layout::of::<u64>()).unwrap();
    layout.push(0x48, Layout::of::<[f32; 6]>()).unwrap();
    layout.push(0x60, Layout::of::<u8>()).unwrap();
    assert_eq!(
        layout
            .push(0x61, Layout::bytes(4).array(5).unwrap())
            .unwrap(),
        0
    );
    assert_eq!(layout.push(0x78, Layout::of::<[f32; 3]>()).unwrap(), 0);
    let (layout, tail) = finish_class_layout(layout, 0x90, false).unwrap();
    assert_eq!(
        layout,
        Layout {
            size: 0x90,
            alignment: 8
        }
    );
    assert_eq!(tail, 0xC);
}

#[test]
fn sdk_render_target_array_does_not_inherit_rtti_overalignment() {
    let mut layout = StructLayout::new(1).unwrap();
    layout.push(0, Layout::bytes(3)).unwrap();
    layout.push(3, Layout::bytes(8).array(8).unwrap()).unwrap();
    assert_eq!(
        finish_class_layout(layout, 0x43, false).unwrap(),
        (Layout::bytes(0x43), 0)
    );
}

#[test]
fn layout_validation_reports_multiple_failures() {
    assert!(report_layout_errors(&[]).is_ok());
    let errors = [
        anyhow!("first incompatible layout").context("laying out First"),
        anyhow!("second incompatible layout").context("laying out Second"),
    ];
    let error = report_layout_errors(&errors).unwrap_err();
    assert!(error.to_string().contains("2 error(s)"));
}

#[test]
fn sdk_anim_fallback_frame_desc_uses_u16_alignment() {
    #[derive(Debug)]
    #[repr(C)]
    struct AnimFallbackFrameDesc {
        positions: u16,
        rotations: u16,
        float_tracks: u16,
    }

    let alignment = size_compatible_alignment(6, 4).unwrap();
    let mut layout = StructLayout::new(alignment).unwrap();
    for offset in [0, 2, 4] {
        layout.push(offset, Layout::of::<u16>()).unwrap();
    }
    assert_eq!(
        finish_class_layout(layout, 6, false).unwrap(),
        (Layout::of::<AnimFallbackFrameDesc>(), 0)
    );
    assert_eq!(alignment, 2);
}

#[test]
fn packed_enum_resolution_controls_both_type_and_padding() {
    let field = ResolvedType {
        rust_type: RustType::Named("TestEnum"),
        layout: Layout::of::<u32>(),
    }
    .at_offset(5, true);
    assert!(matches!(field.rust_type, RustType::Bytes(4)));
    assert_eq!(field.layout, Layout::bytes(4));
    let mut layout = StructLayout::new(1).unwrap();
    layout.push(0, Layout::bytes(3)).unwrap();
    assert_eq!(layout.push(5, field.layout).unwrap(), 2);
    assert_eq!(
        finish_class_layout(layout, 9, false).unwrap().0,
        Layout::bytes(9)
    );

    let field = ResolvedType {
        rust_type: RustType::Named("u32"),
        layout: Layout::of::<u32>(),
    }
    .at_offset(5, false);
    assert!(StructLayout::new(1).unwrap().push(5, field.layout).is_err());
}

#[test]
fn native_size_mismatch_is_rejected_but_holder_tail_rounding_is_allowed() {
    let mut native = StructLayout::new(4).unwrap();
    native.push(0, Layout::bytes(5)).unwrap();
    assert!(finish_class_layout(native, 5, false).is_err());

    let mut holder = StructLayout::new(1).unwrap();
    holder.push(0, Layout::of::<u64>()).unwrap();
    holder.push(8, Layout::of::<bool>()).unwrap();
    assert_eq!(
        finish_class_layout(holder, 10, true).unwrap(),
        (
            Layout {
                size: 16,
                alignment: 8
            },
            1
        )
    );
}

#[test]
fn mapping_keys_and_alignment_overrides_are_unique() {
    let mut names = HashSet::new();
    for (name, _) in constants::KNOWN_TYPE_MAPPING {
        assert!(names.insert(name), "duplicate known type {name}");
    }
    names.clear();
    for (name, alignment) in constants::ALIGNMENT_OVERRIDES {
        assert!(names.insert(name), "duplicate alignment override {name}");
        assert!(alignment.is_power_of_two());
    }
}

#[test]
fn padding_names_are_only_reserved_for_nonzero_padding() {
    let mut count = 0;
    let mut names = HashSet::new();
    assert!(Padding::new(0, &mut count, &mut names).is_none());
    assert_eq!(count, 0);
    assert!(names.is_empty());
    let first = Padding::new(1, &mut count, &mut names).unwrap();
    assert_eq!(first.name, "_padding0");
    assert_eq!(count, 1);
}

#[test]
fn generated_names_include_suffix_and_padding_collisions() {
    let mut names = HashSet::new();
    let fields = ["foo", "Foo", "foo1", "base", "base", "_padding0"]
        .map(|name| unique_name(sanitize_field(name, 0), &mut names));
    assert_eq!(
        fields,
        ["foo", "foo1", "foo11", "base", "base1", "padding0"]
    );
    let mut count = 0;
    names.insert("_padding0".into());
    let padding = Padding::new(3, &mut count, &mut names).unwrap();
    assert_eq!(padding.name, "_padding01");
    let mut output = Vec::new();
    padding.write(&mut output).unwrap();
    assert_eq!(
        String::from_utf8(output).unwrap(),
        "    pub _padding01: [u8; 0x3],\n"
    );
}
