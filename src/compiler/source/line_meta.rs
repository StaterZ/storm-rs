use std::{ptr, fmt::{Display, Debug}};
use super::{SourceFile, Pos, Range, RangeMeta, Line};

#[derive(Clone)]
pub struct LineMeta<'a> {
	pub line: Line,
	pub file: &'a SourceFile,
}

impl<'a> LineMeta<'a> {
	pub fn range(&self) -> RangeMeta<'a> {
		let next_line = self.line.index() + 1;

		let lines_begin_indices = self.file.get_lines_begin_indices();
		let begin = lines_begin_indices[self.line.index()];
		let end = if next_line < lines_begin_indices.len() {
			lines_begin_indices[next_line]
		} else {
			Pos::new(self.file.chars().len())
		};
		
		Range{
			begin,
			end,
		}.to_meta(self.file)
	}

	fn assert_safe(&self, other: &Self) {
		debug_assert!(ptr::eq(self.file, other.file));
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
