use std::{
	fmt::Display,
	ops::{Add, Sub},
};

use super::{
	ColumnMeta,
	Document,
};

#[derive(Debug, Clone, Copy)]
pub struct Column(usize);

impl Column {
	pub fn new(index: usize) -> Self {
		Self(index)
	}

	pub fn index(&self) -> usize {
		self.0
	}

	pub fn to_meta(self, document: &Document) -> ColumnMeta {
		ColumnMeta {
			column: self,
			document,
		}
	}
}

impl Display for Column {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.index() + 1)
	}
}

impl PartialEq<usize> for Column {
	fn eq(&self, other: &usize) -> bool {
		self.index() == *other
	}
}

impl PartialOrd<usize> for Column {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.index().partial_cmp(other)
	}
}

impl Add<usize> for Column {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		Self::new(self.index() + rhs)
	}
}

impl Sub<usize> for Column {
	type Output = Self;

	fn sub(self, rhs: usize) -> Self::Output {
		Self::new(self.index() - rhs)
	}
}
