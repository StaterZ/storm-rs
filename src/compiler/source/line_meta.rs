use std::{ptr, fmt::{Display, Debug}};
use super::{Document, Range, RangeMeta, Line};

#[derive(Clone)]
pub struct LineMeta<'a> {
	pub line: Line,
	pub document: &'a Document,
}

impl<'a> LineMeta<'a> {
	pub fn range(&self) -> RangeMeta<'a> {
		let next_line = self.line.index() + 1;

		let lines_begin_indices = self.document.get_lines_begin_indices();
		let begin = lines_begin_indices[self.line.index()];
		let end = if next_line < lines_begin_indices.len() {
			lines_begin_indices[next_line]
		} else {
			self.document.get_eof().pos
		};
		
		Range{
			begin,
			end,
		}.to_meta(self.document)
	}

	fn assert_safe(&self, other: &Self) {
		debug_assert!(ptr::eq(self.document, other.document));
	}
}

impl<'a> Display for LineMeta<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.line, f)
    }
}

impl<'a> Debug for LineMeta<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.line, f)
    }
}

impl<'a> PartialEq for LineMeta<'a> {
	fn eq(&self, other: &Self) -> bool {
		self.assert_safe(other);
		self.line.index() == other.line.index()
	}
}

impl<'a> PartialOrd for LineMeta<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.assert_safe(other);
        self.line.index().partial_cmp(&other.line.index())
    }
}
