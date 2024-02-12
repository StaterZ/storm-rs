use std::ops::{Add, Sub};

use super::{Document, PosMeta, Range};

#[derive(Debug, Clone, Copy)]
pub struct Pos(usize);

impl Pos {
	pub fn new(inner: usize) -> Pos {
		Self(inner)
	}

	pub fn to_meta(self, document: &Document) -> PosMeta {
		PosMeta {
			pos: self,
			document,
		}
	}

	pub fn char_index(&self) -> usize {
		self.0
	}

	pub fn to_range(&self) -> Range {
		Range {
			begin: *self,
			end: *self + 1,
		}
	}
}

impl PartialEq<usize> for Pos {
	fn eq(&self, other: &usize) -> bool {
		self.char_index() == *other
	}
}

impl PartialOrd<usize> for Pos {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.char_index().partial_cmp(other)
	}
}

impl Add<usize> for Pos {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		Pos::new(self.char_index() + rhs)
	}
}

impl Sub<usize> for Pos {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		Pos::new(self.char_index() - rhs)
	}
}
