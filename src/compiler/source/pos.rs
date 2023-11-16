use super::{SourceFile, SourcePosMeta};

#[derive(Debug, Clone, Copy)]
pub struct SourcePos(usize);

impl SourcePos {
	pub fn new(inner: usize) -> SourcePos {
		Self(inner)
	}

	pub fn to_meta(self, file: &SourceFile) -> SourcePosMeta {
		SourcePosMeta {
			pos: self,
			file,
		}
	}

	pub fn char_index(&self) -> usize {
		self.0
	}
}
