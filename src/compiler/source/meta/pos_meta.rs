use std::{
	fmt::{Display, Debug},
	ops::{Add, Sub},
	ptr,
};

use more_asserts::*;

use super::{
	super::{
		Pos,
		Column,
	},
	DocumentMeta,
	LineMeta,
	RangeMeta,
};

#[derive(Clone, Copy)]
pub struct PosMeta<'a> {
	pub pos: Pos,
	pub document: &'a DocumentMeta<'a>,
}

impl<'a> PosMeta<'a> {
	pub fn line(&self) -> Option<LineMeta<'a>> {
		(!self.is_eof()).then(|| self.line_raw())
	}

	pub fn line_raw(&self) -> LineMeta<'a> {
		self.document.get_line(self)
	}

	pub fn column(&self) -> Option<Column> {
		(!self.is_eof()).then(|| self.column_raw())
	}

	pub fn column_raw(&self) -> Column {
		Column::new(*self - self.line_raw().range().get_begin())
	}

	pub fn is_eof(&self) -> bool {
		let eof = self.document.get_eof();
		debug_assert_le!(*self, eof);
		*self == eof
	}

	pub fn to_range(&self) -> RangeMeta<'_> {
		self.pos.to_range().with_meta(self.document)
	}

	pub fn byte_index(&self) -> usize {
		self.document.get_char_to_byte(self)
	}

	fn assert_safe(self, other: &Self) {
		debug_assert!(ptr::eq(self.document, other.document));
	}
}

impl<'a> Display for PosMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.is_eof() {
			write!(f, "EOF")
		} else {
			write!(f, "{}:{}", self.line_raw(), self.column_raw())
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
		self.pos.char_index() == other.pos.char_index()
	}
}

impl<'a> PartialEq<usize> for PosMeta<'a> {
	fn eq(&self, other: &usize) -> bool {
		self.pos.eq(other)
	}
}

impl<'a> PartialOrd for PosMeta<'a> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.assert_safe(other);
		self.pos.char_index().partial_cmp(&other.pos.char_index())
	}
}

impl<'a> PartialOrd<usize> for PosMeta<'a> {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.pos.partial_cmp(other)
	}
}

impl<'a> Ord for PosMeta<'a> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.assert_safe(other);
		self.pos.char_index().cmp(&other.pos.char_index())
	}
}

impl<'a> Add<usize> for PosMeta<'a> {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		(self.pos + rhs).with_meta(self.document)
	}
}

impl<'a> Sub for PosMeta<'a> {
	type Output = usize;

	fn sub(self, rhs: Self) -> Self::Output {
		self.assert_safe(&rhs);
		self.pos.char_index() - rhs.pos.char_index()
	}
}

impl<'a> Sub<usize> for PosMeta<'a> {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		(self.pos - rhs).with_meta(self.document)
	}
}
