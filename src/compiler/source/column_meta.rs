use std::fmt::{Display, Debug};

use super::{
	Document,
	Column,
};

#[derive(Clone)]
pub struct ColumnMeta<'a> {
	pub column: Column,
	pub document: &'a Document,
}

impl<'a> Display for ColumnMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", &self.column)
	}
}

impl<'a> Debug for ColumnMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", &self.column)
	}
}
