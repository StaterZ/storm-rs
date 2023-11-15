use super::{SourcePos, SourceFile, SourceRangeMeta};

#[derive(Debug, Clone)]
pub struct SourceRange {
	pub begin: SourcePos,
	pub end: SourcePos,
}

impl SourceRange {
	pub fn to_meta(self, file: &SourceFile) -> SourceRangeMeta {
		SourceRangeMeta {
			range: self,
			file,
		}
	}

	pub fn get_length(&self) -> usize {
		self.end - self.begin
	}
}
