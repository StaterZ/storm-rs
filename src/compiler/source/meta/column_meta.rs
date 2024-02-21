use std::{
	fmt::{Display, Debug},
	ptr,
};

use super::super::{
	Document,
	Column,
};

#[derive(Clone, Copy)]
pub struct ColumnMeta<'a> {
	pub column: Column,
	pub document: &'a Document,
}

impl<'a> ColumnMeta<'a> {
	fn assert_safe(&self, other: &Self) {
		debug_assert!(ptr::eq(self.document, other.document));
	}
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
