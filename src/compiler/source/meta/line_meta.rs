use std::{
	fmt::{Debug, Display},
	ops::{Add, Deref, DerefMut, Sub},
	ptr,
};

use super::super::*;

#[derive(Clone, Copy)]
pub struct LineMeta<'a> {
	line: Line,
	pub document: &'a DocumentMeta<'a>,
}

impl<'a> LineMeta<'a> {
	pub(in super::super) fn new_with_document(line: Line, document: &'a DocumentMeta) -> Self {
		Self {
			line,
			document,
		}
	}

	pub fn range(&self) -> RangeMeta<'a> {
		let begin = self.get_line_begin();
		
		let next_line = *self + 1;
		let end = if next_line < self.document.get_num_lines() {
			next_line.get_line_begin()
		} else {
			self.document.eof()
		};
		
		Range::new(begin, end).with_meta(self.document)
	}
	
	fn get_line_begin(&self) -> super::PosMeta<'_> {
		self.document.lines_begin[self.index()].with_meta(self.document)
	}
}

impl Deref for LineMeta<'_> {
	type Target = Line;

	fn deref(&self) -> &Self::Target {
		&self.line
	}
}

impl DerefMut for LineMeta<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.line
	}
}

impl Display for LineMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.line)
	}
}

impl Debug for LineMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.line)
	}
}

impl PartialEq for LineMeta<'_> {
	fn eq(&self, other: &Self) -> bool {
		debug_assert!(ptr::eq(self.document, other.document));
		self.index() == other.index()
	}
}

impl PartialEq<usize> for LineMeta<'_> {
	fn eq(&self, other: &usize) -> bool {
		self.index() == *other
	}
}

impl Eq for LineMeta<'_> { }

impl PartialOrd for LineMeta<'_> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		debug_assert!(ptr::eq(self.document, other.document));
		self.index().partial_cmp(&other.index())
	}
}

impl PartialOrd<usize> for LineMeta<'_> {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.index().partial_cmp(other)
	}
}

impl Ord for LineMeta<'_> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		debug_assert!(ptr::eq(self.document, other.document));
		self.index().cmp(&other.index())
	}
}

impl Add<usize> for LineMeta<'_> {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		(self.line + rhs).with_meta(self.document)
	}
}

impl Sub for LineMeta<'_> {
	type Output = usize;

	fn sub(self, rhs: Self) -> Self::Output {
		debug_assert!(ptr::eq(self.document, rhs.document));
		self.index() - rhs.index()
	}
}

impl Sub<usize> for LineMeta<'_> {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		(self.line - rhs).with_meta(self.document)
	}
}
