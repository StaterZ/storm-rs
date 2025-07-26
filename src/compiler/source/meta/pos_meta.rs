use std::{
	fmt::{Debug, Display},
	ops::Deref,
	ptr,
};

use super::super::*;

#[derive(Clone, Copy)]
pub struct PosMeta<'a> {
	pos: Pos,
	pub document: &'a DocumentMeta<'a>,
}

impl<'a> PosMeta<'a> {
	pub(in super::super) fn new_with_document(pos: Pos, document: &'a DocumentMeta) -> Self {
		Self {
			pos,
			document,
		}
	}

	pub fn line(&self) -> LineMeta<'a> {
		let line_index = match self.document.lines_begin.binary_search_by_key(self, |key| key.with_meta(self.document)) {
			Ok(line_index) => line_index,
			Err(next_line_index) => next_line_index - 1,
		};

		Line::new(line_index).with_meta(self.document)
	}

	pub fn column(&self) -> Column {
		let range = self
			.line()
			.range();
		let range = Range::new(range.get_begin(), self.clone())
			.with_meta(self.document);
		Column::new(range
			.get_str()
			.chars()
			.count())
	}

	pub fn char_index(&self) -> usize {
		self.document.byte_to_char[self.byte_index()]
	}

	pub fn as_range(&self) -> RangeMeta<'_> {
		let content = self.document.get_content();
		let end = Pos::new(content[self.byte_index()..]
			.char_indices()
			.nth(1)
			.map_or(content.len(), |(i, _)| i));
		Range { begin: **self, end }.with_meta(self.document)
	}

	pub fn add_byte_offset(&self, offset: usize) -> Self {
		self.pos.add_byte_offset(offset).with_meta(self.document)
	}

	fn assert_safe(self, other: &Self) {
		debug_assert!(ptr::eq(self.document, other.document));
	}
}

impl<'a> Deref for PosMeta<'a> {
	type Target = Pos;

	fn deref(&self) -> &Self::Target {
		&self.pos
	}
}

impl<'a> Display for PosMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if *self == self.document.eof() {
			write!(f, "EOF")
		} else {
			write!(f, "{}:{}", self.line(), self.column())
		}
	}
}

impl<'a> Debug for PosMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.pos)
	}
}

impl<'a> Eq for PosMeta<'a> { }

impl<'a> PartialEq for PosMeta<'a> {
	fn eq(&self, other: &Self) -> bool {
		self.assert_safe(other);
		self.byte_index() == other.byte_index()
	}
}

impl<'a> PartialOrd for PosMeta<'a> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.assert_safe(other);
		self.byte_index().partial_cmp(&other.byte_index())
	}
}

impl<'a> Ord for PosMeta<'a> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.assert_safe(other);
		self.byte_index().cmp(&other.byte_index())
	}
}
