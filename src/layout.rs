use std::mem;

use anyhow::{ensure, Context, Result};

const MAX_ALIGNMENT: u32 = 1 << 29;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Layout {
    pub size: u32,
    pub alignment: u32,
}

impl Layout {
    pub(crate) const fn of<T>() -> Self {
        Self {
            size: mem::size_of::<T>() as u32,
            alignment: mem::align_of::<T>() as u32,
        }
    }

    pub(crate) fn bytes(size: u32) -> Self {
        Self { size, alignment: 1 }
    }

    pub(crate) fn array(self, count: u32) -> Result<Self> {
        self.validate()?;
        Ok(Self {
            size: self
                .size
                .checked_mul(count)
                .context("array size overflow")?,
            alignment: self.alignment,
        })
    }

    pub(crate) fn static_array(self, count: u32) -> Result<Self> {
        let entries = self.array(count)?;
        let count_layout = Self::of::<u32>();
        let count_offset = round_up(entries.size, count_layout.alignment)?;
        let end = count_offset
            .checked_add(count_layout.size)
            .context("static array count offset overflow")?;
        let alignment = entries.alignment.max(count_layout.alignment);
        Ok(Self {
            size: round_up(end, alignment)?,
            alignment,
        })
    }

    fn validate(self) -> Result<()> {
        validate_alignment(self.alignment)?;
        ensure!(
            self.size.is_multiple_of(self.alignment),
            "layout size {} is not a multiple of alignment {}",
            self.size,
            self.alignment
        );
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct StructLayout {
    end: u32,
    alignment: u32,
}

impl StructLayout {
    pub(crate) fn new(min_alignment: u32) -> Result<Self> {
        validate_alignment(min_alignment)?;
        Ok(Self {
            end: 0,
            alignment: min_alignment,
        })
    }

    pub(crate) fn push(&mut self, offset: u32, field: Layout) -> Result<u32> {
        field.validate()?;
        ensure!(
            offset >= self.end,
            "field offset {offset} overlaps previous field ending at {}",
            self.end
        );
        ensure!(
            offset.is_multiple_of(field.alignment),
            "field offset {offset} is not aligned to {}",
            field.alignment
        );
        let natural_offset = round_up(self.end, field.alignment)?;
        let padding = if offset == natural_offset {
            0
        } else {
            offset - self.end
        };
        let end = offset
            .checked_add(field.size)
            .context("field end overflow")?;
        self.end = end;
        self.alignment = self.alignment.max(field.alignment);
        Ok(padding)
    }

    pub(crate) fn finish(self, target_size: u32) -> Result<(Layout, u32)> {
        ensure!(
            self.end <= target_size,
            "fields end at {}, beyond target size {target_size}",
            self.end
        );
        // Keep all requested tail bytes explicit, even when Rust would add them implicitly.
        let tail = target_size - self.end;
        Ok((
            Layout {
                size: round_up(target_size, self.alignment)?,
                alignment: self.alignment,
            },
            tail,
        ))
    }
}

pub(crate) fn size_compatible_alignment(size: u32, alignment: u32) -> Result<u32> {
    validate_alignment(alignment)?;
    // RTTI class alignment can describe allocation rather than native storage.
    // A nonzero Rust type's alignment must divide its size. This is only a cap:
    // member alignments and explicit SDK corrections are still authoritative.
    Ok(if size == 0 {
        alignment
    } else {
        alignment.min(1 << size.trailing_zeros())
    })
}

fn validate_alignment(alignment: u32) -> Result<()> {
    ensure!(
        alignment.is_power_of_two() && alignment <= MAX_ALIGNMENT,
        "alignment {alignment} must be a power of two in 1..={MAX_ALIGNMENT}"
    );
    Ok(())
}

fn round_up(size: u32, alignment: u32) -> Result<u32> {
    let remainder = size % alignment;
    if remainder == 0 {
        Ok(size)
    } else {
        size.checked_add(alignment - remainder)
            .context("aligned size overflow")
    }
}

#[cfg(test)]
mod tests {
    use super::{size_compatible_alignment, Layout, StructLayout, MAX_ALIGNMENT};

    #[derive(Debug)]
    #[repr(C)]
    struct StaticArray<T, const N: usize> {
        entries: [T; N],
        count: u32,
    }

    #[derive(Debug)]
    #[repr(C, align(16))]
    struct Aligned(u8);

    #[derive(Debug)]
    #[repr(C, packed)]
    struct TweakDbId {
        hash: u32,
        length: u8,
    }

    #[derive(Debug)]
    #[repr(u8)]
    #[allow(dead_code)]
    enum ByteEnum {
        First,
        Second,
    }

    #[derive(Debug)]
    #[repr(C)]
    struct ByteFields {
        bitfield: u8,
        enum_padding: [u8; 3],
        value: ByteEnum,
        tail: [u8; 3],
    }

    #[derive(Debug)]
    #[repr(C)]
    struct BoolHolder8 {
        value: bool,
        tail: [u8; 7],
    }

    #[derive(Debug)]
    #[repr(C, align(1))]
    struct CannotLowerAlignment {
        value: u64,
    }

    #[derive(Debug)]
    #[repr(C)]
    struct NaturallyPadded {
        byte: u8,
        value: u32,
    }

    #[test]
    fn reported_alignment_is_capped_by_nonzero_storage_size() {
        for (size, expected) in [(1, 1), (2, 2), (3, 1), (5, 1), (6, 2), (11, 1), (86, 2)] {
            assert_eq!(size_compatible_alignment(size, 4).unwrap(), expected);
        }
        for (size, alignment) in [(0, 16), (8, 8), (12, 4), (16, 16), (32, 16)] {
            assert_eq!(
                size_compatible_alignment(size, alignment).unwrap(),
                alignment
            );
        }
        assert_eq!(size_compatible_alignment(6, 1).unwrap(), 1);
        for invalid in [0, 3, 6, 1 << 30] {
            assert!(size_compatible_alignment(1, invalid).is_err());
        }

        // The cap cannot fix an incompatible member; exact-size checking must fail.
        let mut layout = StructLayout::new(size_compatible_alignment(6, 4).unwrap()).unwrap();
        layout.push(0, Layout::of::<u32>()).unwrap();
        assert_eq!(layout.finish(6).unwrap().0.size, 8);
    }

    fn check_arrays<T, const N: usize>() {
        assert_eq!(
            Layout::of::<T>().array(N as u32).unwrap(),
            Layout::of::<[T; N]>()
        );
        assert_eq!(
            Layout::of::<T>().static_array(N as u32).unwrap(),
            Layout::of::<StaticArray<T, N>>()
        );
    }

    #[test]
    fn arrays_match_rust_layouts() {
        check_arrays::<u8, 0>();
        check_arrays::<u8, 1>();
        check_arrays::<u8, 3>();
        check_arrays::<u16, 3>();
        check_arrays::<u32, 4>();
        check_arrays::<u64, 0>();
        check_arrays::<u64, 3>();
        check_arrays::<Aligned, 0>();
        check_arrays::<Aligned, 2>();
        check_arrays::<TweakDbId, 3>();
        check_arrays::<(), 0>();
        check_arrays::<(), 7>();
        check_arrays::<[Aligned; 0], 3>();
    }

    #[test]
    fn minimum_alignment_cannot_lower_field_alignment() {
        for minimum in [1, 2, 4] {
            let mut builder = StructLayout::new(minimum).unwrap();
            assert_eq!(builder.push(0, Layout::of::<u64>()).unwrap(), 0);
            let (layout, tail) = builder.finish(8).unwrap();
            assert_eq!(layout, Layout::of::<CannotLowerAlignment>());
            assert_eq!(tail, 0);
        }
        let mut builder = StructLayout::new(16).unwrap();
        builder.push(0, Layout::of::<u8>()).unwrap();
        assert_eq!(builder.finish(1).unwrap(), (Layout::of::<Aligned>(), 0));
    }

    #[test]
    fn natural_padding_is_implicit() {
        let mut builder = StructLayout::new(1).unwrap();
        assert_eq!(builder.push(0, Layout::of::<u8>()).unwrap(), 0);
        assert_eq!(builder.push(4, Layout::of::<u32>()).unwrap(), 0);
        assert_eq!(
            builder.finish(8).unwrap(),
            (Layout::of::<NaturallyPadded>(), 0)
        );
    }

    #[test]
    fn extra_padding_includes_the_entire_gap() {
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::of::<u8>()).unwrap();
        assert_eq!(builder.push(8, Layout::of::<u32>()).unwrap(), 7);
        assert_eq!(
            builder.finish(12).unwrap(),
            (
                Layout {
                    size: 12,
                    alignment: 4
                },
                0
            )
        );
    }

    #[test]
    fn bitfields_and_byte_enums_need_explicit_padding() {
        let mut builder = StructLayout::new(1).unwrap();
        assert_eq!(builder.push(0, Layout::of::<u8>()).unwrap(), 0);
        assert_eq!(builder.push(4, Layout::of::<ByteEnum>()).unwrap(), 3);
        assert_eq!(builder.finish(8).unwrap(), (Layout::of::<ByteFields>(), 3));
    }

    #[test]
    fn packed_field_uses_its_actual_alignment() {
        assert_eq!(
            Layout::of::<TweakDbId>(),
            Layout {
                size: 5,
                alignment: 1
            }
        );
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::of::<u8>()).unwrap();
        assert_eq!(builder.push(1, Layout::of::<TweakDbId>()).unwrap(), 0);
        assert_eq!(builder.finish(6).unwrap(), (Layout::bytes(6), 0));
    }

    #[test]
    fn all_tail_bytes_are_explicit() {
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::of::<bool>()).unwrap();
        assert_eq!(builder.finish(8).unwrap(), (Layout::of::<BoolHolder8>(), 7));

        let mut builder = StructLayout::new(8).unwrap();
        builder.push(0, Layout::of::<bool>()).unwrap();
        assert_eq!(
            builder.finish(8).unwrap(),
            (
                Layout {
                    size: 8,
                    alignment: 8
                },
                7
            )
        );
    }

    #[test]
    fn empty_and_zero_sized_fields_are_supported() {
        assert_eq!(
            StructLayout::new(1).unwrap().finish(0).unwrap(),
            (Layout::bytes(0), 0)
        );
        assert_eq!(
            StructLayout::new(16).unwrap().finish(0).unwrap(),
            (
                Layout {
                    size: 0,
                    alignment: 16
                },
                0
            )
        );
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::of::<[Aligned; 0]>()).unwrap();
        builder.push(0, Layout::of::<()>()).unwrap();
        assert_eq!(
            builder.finish(0).unwrap(),
            (Layout::of::<[Aligned; 0]>(), 0)
        );
    }

    #[test]
    fn finish_rounds_target_up_without_hiding_tail_bytes() {
        let mut builder = StructLayout::new(4).unwrap();
        builder.push(0, Layout::of::<u8>()).unwrap();
        assert_eq!(
            builder.finish(5).unwrap(),
            (
                Layout {
                    size: 8,
                    alignment: 4
                },
                4
            )
        );
    }

    #[test]
    fn invalid_alignments_and_layouts_are_rejected() {
        for alignment in [0, 3, 6, MAX_ALIGNMENT + 1, 1 << 30, 1 << 31] {
            assert!(StructLayout::new(alignment).is_err());
            let field = Layout { size: 0, alignment };
            assert!(field.array(0).is_err());
            assert!(field.static_array(0).is_err());
            assert!(StructLayout::new(1).unwrap().push(0, field).is_err());
        }
        let invalid = Layout {
            size: 3,
            alignment: 2,
        };
        assert!(invalid.array(0).is_err());
        assert!(invalid.static_array(0).is_err());
        assert!(StructLayout::new(1).unwrap().push(0, invalid).is_err());
        assert!(StructLayout::new(MAX_ALIGNMENT).is_ok());
    }

    #[test]
    fn overlapping_misaligned_and_out_of_bounds_fields_are_rejected() {
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::of::<u32>()).unwrap();
        assert!(builder.push(3, Layout::of::<u8>()).is_err());
        assert!(builder.push(2, Layout::of::<()>()).is_err());
        assert!(builder.push(5, Layout::of::<u32>()).is_err());
        assert_eq!(builder.push(4, Layout::of::<u8>()).unwrap(), 0);
        assert!(builder.finish(4).is_err());
    }

    #[test]
    fn rejected_fields_do_not_change_builder_state() {
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::of::<u8>()).unwrap();

        assert!(builder.push(2, Layout::of::<u32>()).is_err());
        assert!(builder.push(u32::MAX, Layout::bytes(1)).is_err());
        assert!(builder
            .push(
                4,
                Layout {
                    size: 3,
                    alignment: 4
                }
            )
            .is_err());

        assert_eq!(builder.push(1, Layout::of::<u8>()).unwrap(), 0);
        assert_eq!(builder.finish(2).unwrap(), (Layout::bytes(2), 0));
    }

    #[test]
    fn arithmetic_overflows_are_rejected() {
        assert!(Layout::of::<u32>().array(u32::MAX).is_err());
        assert!(Layout::bytes(u32::MAX).static_array(1).is_err());
        assert!(Layout::bytes(u32::MAX - 3).static_array(1).is_err());
        assert!(Layout {
            size: 0x8000_0000,
            alignment: 8
        }
        .static_array(1)
        .is_ok());
        assert!(Layout {
            size: u32::MAX - 7,
            alignment: 8
        }
        .static_array(1)
        .is_err());

        let mut builder = StructLayout::new(1).unwrap();
        assert!(builder.push(u32::MAX, Layout::bytes(1)).is_err());
        assert_eq!(builder.finish(0).unwrap(), (Layout::bytes(0), 0));
        assert!(StructLayout::new(4).unwrap().finish(u32::MAX).is_err());
    }

    #[test]
    fn maximum_byte_size_does_not_overflow_when_already_aligned() {
        let mut builder = StructLayout::new(1).unwrap();
        builder.push(0, Layout::bytes(u32::MAX)).unwrap();
        assert_eq!(
            builder.finish(u32::MAX).unwrap(),
            (Layout::bytes(u32::MAX), 0)
        );
        assert_eq!(
            Layout::bytes(u32::MAX).array(1).unwrap(),
            Layout::bytes(u32::MAX)
        );
        assert_eq!(
            Layout::of::<[Aligned; 0]>().array(u32::MAX).unwrap().size,
            0
        );
    }
}
