use std::{fmt::{Display, Debug}, ops::{Deref, DerefMut}};

use super::super::*;

#[derive(Clone, Copy)]
pub struct SourcedMeta<'a, T> {
	sourced: Sourced<T>,
	pub document: &'a DocumentMeta<'a>,
}

impl<'a, T> SourcedMeta<'a, T> {
	pub fn new_with_document(sourced: Sourced<T>, document: &'a DocumentMeta) -> Self {
		Self {
			sourced,
			document,
		}
	}

	pub fn source(&self) -> RangeMeta<'a> {
		self.sourced.source().with_meta(self.document)
	}
}

impl<'a, T> Deref for SourcedMeta<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.sourced
	}
}

impl<'a, T> DerefMut for SourcedMeta<'a, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.sourced
	}
}

impl<'a, T: Display> Display for SourcedMeta<'a, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}| {}", self.source(), self.sourced.deref())
	}
}

impl<'a, T: Debug> Debug for SourcedMeta<'a, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}| {:?}", self.source(), self.sourced.deref())
	}
}
