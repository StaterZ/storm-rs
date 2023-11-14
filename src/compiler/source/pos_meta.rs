use std::fmt::Display;

use super::{SourcePos, SourceFile, SourceRange, SourceRangeMeta};

#[derive(Debug, Clone)]
pub struct SourcePosMeta<'a> {
	pub pos: SourcePos,
	pub file: &'a SourceFile,
}

impl<'a> SourcePosMeta<'a> {
	pub fn column_index(&self) -> usize {
		self.pos - self.get_line().range.begin
	}

	pub fn column_number(&self) -> usize {
		self.column_index() + 1
	}
	
	pub fn line_index(&self) -> usize {
		self.file.get_line_index(self.pos)
	}

	pub fn line_number(&self) -> usize {
		self.line_index() + 1
	}

	pub fn is_eof(&self) -> bool {
		self.pos >= self.file.get_num_chars()
	}

	pub fn get_line(&self) -> SourceRangeMeta<'a> {
		let line = self.line_index();
		let next_line = line + 1;

		let lines_begin_indices = self.file.get_lines_begin_indices();
		let begin = lines_begin_indices[line];
		let end = if next_line < lines_begin_indices.len() {
			lines_begin_indices[next_line]
		} else {
			SourcePos::new(self.file.get_num_chars())
		};
		
		SourceRange{
			begin,
			end,
		}.to_meta(self.file)
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
