use std::{fmt::{Display, Debug}, ops::{Add, Sub}, ptr};

use more_asserts::debug_assert_le;

use super::{SourcePos, SourceFile, Line, Column, LineMeta};

#[derive(Clone, Copy)]
pub struct SourcePosMeta<'a> {
	pub pos: SourcePos,
	pub file: &'a SourceFile,
}

impl<'a> SourcePosMeta<'a> {
	fn new(&self, pos: SourcePos) -> Self {
		Self {
			pos,
			file: self.file,
		}
	}

	pub fn line(&self) -> Option<LineMeta<'a>> {
		(!self.is_eof()).then_some(LineMeta{
			line: Line::new(self.file.get_line_index(self)),
			file: self.file},
		)
	}

	pub fn column(&self) -> Option<Column> {
		(!self.is_eof()).then_some(Column::new(*self - self.line().unwrap().range().get_begin()))
	}

	pub fn is_eof(&self) -> bool {
		let eof = self.file.get_eof();
		debug_assert_le!(*self, eof);
		*self == eof
	}

	pub fn byte_index(&self) -> usize {
		self.file.get_char_to_byte(self)
	}

	fn assert_safe(self, other: &Self) {
		debug_assert!(ptr::eq(self.file, other.file));
	}
}

impl<'a> Display for SourcePosMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.is_eof() {
			write!(f, "EOF")
		} else {
			write!(f, "{}:{}", self.line().unwrap(), self.column().unwrap())
		}
	}
}

impl<'a> Debug for SourcePosMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.pos.fmt(f)
	}
}

impl<'a> PartialEq for SourcePosMeta<'a> {
    fn eq(&self, other: &Self) -> bool {
		self.assert_safe(other);
        self.pos.char_index() == other.pos.char_index()
    }
}

impl<'a> Eq for SourcePosMeta<'a> { }

impl<'a> PartialEq<usize> for SourcePosMeta<'a> {
    fn eq(&self, other: &usize) -> bool {
        self.pos.char_index() == *other
    }
}

impl<'a> PartialOrd for SourcePosMeta<'a> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.assert_safe(other);
		self.pos.char_index().partial_cmp(&other.pos.char_index())
    }
}

impl<'a> Ord for SourcePosMeta<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.assert_safe(other);
        self.pos.char_index().cmp(&other.pos.char_index())
    }
}

impl<'a> PartialOrd<usize> for SourcePosMeta<'a> {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.pos.char_index().partial_cmp(other)
    }
}

impl<'a> Add<usize> for SourcePosMeta<'a> {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		self.new(SourcePos::new(self.pos.char_index() + rhs))
	}
}

impl<'a> Sub for SourcePosMeta<'a> {
	type Output = usize;

	fn sub(self, rhs: Self) -> Self::Output {
		self.assert_safe(&rhs);
		self.pos.char_index() - rhs.pos.char_index()
	}
}

impl<'a> Sub<usize> for SourcePosMeta<'a> {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		self.new(SourcePos::new(self.pos.char_index() - rhs))
	}
}
