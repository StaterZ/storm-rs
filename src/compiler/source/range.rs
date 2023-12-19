use std::ptr;

use more_asserts::debug_assert_le;

use super::{SourceFile, Pos, PosMeta, RangeMeta};

#[derive(Debug, Clone, Copy)]
pub struct Range {
	pub begin: Pos,
	pub end: Pos,
}

impl Range {
	pub fn new(begin: PosMeta, end: PosMeta) -> Self {
		debug_assert!(ptr::eq(begin.file, end.file));
		debug_assert_le!(begin, end);
		Self {
			begin: begin.pos,
			end: end.pos,
		}
	}

	pub fn to_meta(self, file: &SourceFile) -> RangeMeta {
		RangeMeta {
			range: self,
			file,
		}
	}
}
