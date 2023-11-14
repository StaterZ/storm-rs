use std::ops::{Add, Sub};

use super::{SourceFile, SourcePosMeta};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SourcePos(usize);

impl SourcePos {
	pub fn new(inner: usize) -> SourcePos {
		Self(inner)
	}

	pub fn to_meta<'a>(self, file: &'a SourceFile) -> SourcePosMeta<'a> {
		SourcePosMeta {
			pos: self,
			file,
		}
	}

	pub fn get_inner(&self) -> usize {
		self.0
	}
}

impl Into<usize> for SourcePos {
	fn into(self) -> usize {
		self.get_inner()
	}
}

impl PartialEq<usize> for SourcePos {
    fn eq(&self, other: &usize) -> bool {
        self.get_inner() == *other
    }
}

impl PartialOrd<usize> for SourcePos {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.get_inner().partial_cmp(other)
    }
}

impl Add<usize> for SourcePos {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		Self(self.get_inner() + rhs)
	}
}

impl Sub for SourcePos {
	type Output = usize;

	fn sub(self, rhs: Self) -> Self::Output {
		self.get_inner() - rhs.get_inner()
	}
}

impl Sub<usize> for SourcePos {
	type Output = Option<Self>;

	fn sub(self, rhs: usize) -> Self::Output {
		(self >= rhs).then_some(Self(self.get_inner() - rhs))
	}
}
