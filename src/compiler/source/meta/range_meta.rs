use std::{
	fmt::{Debug, Display}, ops::{Deref, DerefMut}, ptr
};

use super::super::*;

#[derive(Clone, Copy)]
pub struct RangeMeta<'a> {
	range: Range,
	pub document: &'a DocumentMeta<'a>,
}

impl<'a> RangeMeta<'a> {
	pub fn new(begin: PosMeta<'a>, end: PosMeta<'a>) -> Self {
		let document = begin.document; //begin.document == end.document is asserted in Range::new, any of them will do
		Range::new(begin, end).with_meta(&document)
	}

	pub(in super::super) fn new_with_document(range: Range, document: &'a DocumentMeta) -> Self {
		Self {
			range,
			document,
		}
	}

	pub fn get_begin(&self) -> PosMeta<'a> {
		self.range.begin.with_meta(self.document)
	}

	pub fn get_end(&self) -> PosMeta<'a> {
		self.range.end.with_meta(self.document)
	}
	
	pub fn get_last(&self) -> Option<PosMeta<'a>> {
		if self.get_begin() > self.get_end() { return None; }

		self.document
			.get_content()[..self.get_end().byte_index()]
			.char_indices()
			.map(|(i, _)| Pos::new(i).with_meta(self.document))
			.rev()
			.next()
	}

	pub fn get_length(&self) -> usize {
		self.get_end().char_index() - self.get_begin().char_index()
	}

	pub fn get_str(&self) -> &'a str {
		&self.document.get_content()[self.get_begin().byte_index() .. self.get_end().byte_index()]
	}
}

impl Deref for RangeMeta<'_> {
	type Target = Range;

	fn deref(&self) -> &Self::Target {
		&self.range
	}
}

impl DerefMut for RangeMeta<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.range
	}
}

impl Display for RangeMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let begin = self.get_begin();
		let end = self.get_end();

		if begin == end {
			write!(f, "{} (0 sized)", begin)
		} else {
			let last = self.get_last().unwrap(); //SAFETY: unwarp safe due to begin != end, i.e 0<=begin<end, so end>0

			if begin.line() != last.line() {
				write!(f, "{}-{}", begin, last)
			} else if begin == last {
				write!(f, "{}", begin)
			} else {
				write!(f, "{}-{}", begin, last.column())
			}
		}
	}
}

impl Debug for RangeMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.range)
	}
}

impl PartialEq for RangeMeta<'_> {
	fn eq(&self, other: &Self) -> bool {
		debug_assert!(ptr::eq(self.document, other.document));
		self.get_begin() == other.get_begin() && self.get_end() == other.get_end()
	}
}

impl Eq for RangeMeta<'_> { }
