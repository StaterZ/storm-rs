use std::{
	fmt::{Debug, Display}, ops::{Deref, DerefMut}, ptr
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
}

impl Deref for ColumnMeta<'_> {
	type Target = Column;

	fn deref(&self) -> &Self::Target {
		&self.column
	}
}

impl DerefMut for ColumnMeta<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.column
	}
}

impl Display for ColumnMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", &self.column)
	}
}

impl Debug for ColumnMeta<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", &self.column)
	}
}

impl PartialEq for ColumnMeta<'_> {
	fn eq(&self, other: &Self) -> bool {
		debug_assert!(ptr::eq(self.document, other.document));
		self.index() == other.index()
	}
}

impl PartialEq<usize> for ColumnMeta<'_> {
	fn eq(&self, other: &usize) -> bool {
		self.index() == *other
	}
}

impl Eq for ColumnMeta<'_> { }

impl PartialOrd for ColumnMeta<'_> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		debug_assert!(ptr::eq(self.document, other.document));
		self.index().partial_cmp(&other.index())
	}
}

impl PartialOrd<usize> for ColumnMeta<'_> {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		self.index().partial_cmp(other)
	}
}

impl Ord for ColumnMeta<'_> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		debug_assert!(ptr::eq(self.document, other.document));
		self.index().cmp(&other.index())
	}
}
