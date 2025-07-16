use super::{
	DocumentMeta,
	PosMeta,
};

#[derive(Debug, Clone, Copy)]
pub struct Pos(usize);

impl Pos {
	pub(super) fn new(inner: usize) -> Pos {
		Self(inner)
	}
	
	//TODO: remove me
	pub fn new_todo(inner: usize) -> Pos {
		Self(inner)
	}

	pub fn with_meta<'a>(self, document: &'a DocumentMeta<'a>) -> PosMeta<'a> {
		PosMeta::new_with_document(self, document)
	}

	pub fn byte_index(&self) -> usize {
		self.0
	}

	pub fn add_byte_offset(&self, offset: usize) -> Self {
		Self(self.0 + offset)
	}
}
