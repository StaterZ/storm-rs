use std::ops::{Add, Sub};

use super::{
	DocumentMeta,
	PosMeta,
	Range,
};

#[derive(Debug, Clone, Copy)]
pub struct Pos(usize);

impl Pos {
	pub(super) fn new(inner: usize) -> Pos {
		Self(inner)
	}
	
	//TODO: remove me
	pub fn new_todo(inner: usize) -> Pos {
		Self(inner)
	}

	pub fn with_meta<'a>(self, document: &'a DocumentMeta<'a>) -> PosMeta<'a> {
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
		Self::new(self.char_index() + rhs)
	}
}

impl Sub<usize> for Pos {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		Self::new(self.char_index() - rhs)
	}
}
