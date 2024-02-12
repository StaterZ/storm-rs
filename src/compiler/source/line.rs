use std::fmt::Display;

use super::{Document, LineMeta};

#[derive(Debug, Clone)]
pub struct Line(usize);

impl Line {
	pub fn new(index: usize) -> Self {
		Self(index)
	}

	pub fn index(&self) -> usize {
		self.0
	}

	pub fn to_meta(self, document: &Document) -> LineMeta {
		LineMeta {
			line: self,
			document,
		}
	}
}

impl Display for Line {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.index() + 1)
	}
}
