use std::{
	fmt::{Display, Debug},
	ptr,
};

use super::super::*;

#[derive(Clone, Copy)]
pub struct ColumnMeta<'a> {
	column: Column,
	pub document: &'a DocumentMeta<'a>,
}

impl<'a> ColumnMeta<'a> {
	pub(in super::super) fn new_with_document(column: Column, document: &'a DocumentMeta<'a>) -> Self {
		Self {
			column,
			document,
		}
	}

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
