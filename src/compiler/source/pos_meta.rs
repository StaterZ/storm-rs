use std::fmt::Display;

use super::{SourcePos, SourceFile, Line, Column, LineMeta};

#[derive(Debug, Clone)]
pub struct SourcePosMeta<'a> {
	pub pos: SourcePos,
	pub file: &'a SourceFile,
}

impl<'a> SourcePosMeta<'a> {
	pub fn line(&self) -> Option<LineMeta> {
		(!self.is_eof()).then_some(LineMeta{
			line: Line::new(self.file.get_line_index(self.pos)),
			file: self.file},
		)
	}

	pub fn column(&self) -> Option<Column> {
		(!self.is_eof()).then_some(Column::new(self.pos - self.line().unwrap().range().range.begin))
	}

	pub fn is_eof(&self) -> bool {
		self.pos >= self.file.get_num_chars()
	}

	pub fn get_byte_index(&self, pos: SourcePos) -> usize {
		self.file.get_char_to_byte(pos)
	}
}

impl<'a> Display for SourcePosMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.is_eof() {
			write!(f, "EOF")
		} else {
			write!(f, "{}:{}", self.line_number(), self.column_number())
		}
	}
}
