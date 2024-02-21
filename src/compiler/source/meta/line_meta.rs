use std::{
	fmt::{Display, Debug},
	ops::{Add, Sub},
	ptr,
};

use super::{
	super::{
		Document,
		Range,
		Line,
	},
	RangeMeta,
};

#[derive(Clone, Copy)]
pub struct LineMeta<'a> {
	pub line: Line,
	pub document: &'a Document,
}

impl<'a> LineMeta<'a> {
	pub fn range(&self) -> RangeMeta<'a> {
		let begin = self.document.get_line_begin(self);
		
		let next_line = *self + 1;
		let end = if next_line < self.document.get_num_lines() {
			self.document.get_line_begin(&next_line)
		} else {
			self.document.get_eof()
		};
		
		Range::new(begin, end).to_meta(self.document)
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

impl<'a> PartialEq<usize> for LineMeta<'a> {
	fn eq(&self, other: &usize) -> bool {
		self.line.index() == *other
	}
}

impl<'a> PartialOrd for LineMeta<'a> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.assert_safe(other);
		self.line.index().partial_cmp(&other.line.index())
	}
}

impl<'a> PartialOrd<usize> for LineMeta<'a> {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.line.index().partial_cmp(other)
	}
}

impl<'a> Add<usize> for LineMeta<'a> {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		(self.line + rhs).to_meta(self.document)
	}
}

impl<'a> Sub for LineMeta<'a> {
	type Output = usize;

	fn sub(self, rhs: Self) -> Self::Output {
		self.assert_safe(&rhs);
		self.line.index() - rhs.line.index()
	}
}

impl<'a> Sub<usize> for LineMeta<'a> {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		(self.line - rhs).to_meta(self.document)
	}
}
