use std::fmt::Display;

use super::{
	ColumnMeta,
	Document,
};

#[derive(Debug, Clone)]
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