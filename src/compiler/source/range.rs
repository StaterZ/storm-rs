use std::ptr;

use more_asserts::debug_assert_le;

use super::{
	Document,
	Pos,
	PosMeta,
	RangeMeta,
};

#[derive(Debug, Clone, Copy)]
pub struct Range {
	pub begin: Pos,
	pub end: Pos,
}

impl Range {
	pub fn new<'a>(begin: PosMeta<'a>, end: PosMeta<'a>) -> Self {
		debug_assert!(ptr::eq(begin.document, end.document));
		debug_assert_le!(begin, end);
		Self {
			begin: begin.pos,
			end: end.pos,
		}
	}

	pub fn to_meta(self, document: &Document) -> RangeMeta {
		RangeMeta {
			range: self,
			document,
		}
	}
}
