use std::{
	fmt::{Debug, Display},
	ops::{Deref, DerefMut},
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
}

impl Deref for PosMeta<'_> {
	type Target = Pos;

	fn deref(&self) -> &Self::Target {
		&self.pos
	}
}

impl DerefMut for PosMeta<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.pos
	}
}

impl Display for PosMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if *self == self.document.eof() {
			write!(f, "EOF")
		} else {
			write!(f, "{}:{}", self.line(), self.column())
		}
	}
}

impl Debug for PosMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.pos)
	}
}

impl PartialEq for PosMeta<'_> {
	fn eq(&self, other: &Self) -> bool {
		debug_assert!(ptr::eq(self.document, other.document));
		self.byte_index() == other.byte_index()
	}
}

impl Eq for PosMeta<'_> { }

impl PartialOrd for PosMeta<'_> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		debug_assert!(ptr::eq(self.document, other.document));
		self.byte_index().partial_cmp(&other.byte_index())
	}
}

impl Ord for PosMeta<'_> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		debug_assert!(ptr::eq(self.document, other.document));
		self.byte_index().cmp(&other.byte_index())
	}
}
