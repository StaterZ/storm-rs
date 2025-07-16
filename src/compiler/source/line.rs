use std::{
	fmt::Display,
	ops::{Add, Sub},
};

use super::{
	DocumentMeta,
	LineMeta,
};

#[derive(Debug, Clone, Copy)]
pub struct Line(usize);

impl Line {
	pub(super) fn new(index: usize) -> Self {
		Self(index)
	}

	pub fn index(&self) -> usize {
		self.0
	}

	pub fn with_meta<'a>(self, document: &'a DocumentMeta<'a>) -> LineMeta<'a> {
		LineMeta::new_with_document(self, document)
	}
}

impl Display for Line {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.index() + 1)
	}
}

impl PartialEq<usize> for Line {
	fn eq(&self, other: &usize) -> bool {
		self.index() == *other
	}
}

impl PartialOrd<usize> for Line {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.index().partial_cmp(other)
	}
}

impl Add<usize> for Line {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		Self::new(self.index() + rhs)
	}
}

impl Sub<usize> for Line {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		Self::new(self.index() - rhs)
	}
}
