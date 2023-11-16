use std::ptr;

use more_asserts::debug_assert_le;

use super::{SourcePos, SourceFile, SourceRangeMeta, SourcePosMeta};

#[derive(Debug, Clone, Copy)]
pub struct SourceRange {
	pub begin: SourcePos,
	pub end: SourcePos,
}

impl SourceRange {
	pub fn new(begin: SourcePosMeta, end: SourcePosMeta) -> Self {
		debug_assert!(ptr::eq(begin.file, end.file));
		debug_assert_le!(begin, end);
		Self {
			begin: begin.pos,
			end: end.pos,
		}
	}

	pub fn to_meta(self, file: &SourceFile) -> SourceRangeMeta {
		SourceRangeMeta {
			range: self,
			file,
		}
	}
}
