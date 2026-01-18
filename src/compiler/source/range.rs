use std::ptr;

use more_asserts::*;

use super::{
	Pos,
	DocumentMeta,
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
			begin: *begin,
			end: *end,
		}
	}

	pub fn new_zero() -> Self {
		Self {
			begin: Pos::new(0),
			end: Pos::new(0),
		}
	}

	pub fn with_meta<'a>(self, document: &'a DocumentMeta<'a>) -> RangeMeta<'a> {
		RangeMeta::new_with_document(self, document)
	}
}
