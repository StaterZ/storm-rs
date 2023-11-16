use std::ptr;
use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;

use super::{SourcePos, SourceRange, CharsLen, SourcePosMeta};

#[derive(Debug)]
pub struct SourceFile {
	name: String,
	content: String,
	lines_begin_indices: Vec<SourcePos>,
	char_to_byte: Vec<usize>,
}

impl SourceFile {
	pub fn new(name: String, content: String) -> Self {
		let mut char_to_byte = Vec::<usize>::new();
		let lines_begin_indices: Vec<SourcePos> = content
			.char_indices()
			.enumerate()
			.window_option()
			.filter_map_deref(|(prev_c, (ci, (bi, c)))| {
				let prev_c = prev_c.as_ref().map(|(_, (_, prev_c))| prev_c);

				char_to_byte.push(*bi);

				match (prev_c, c) {
					(Some('\r'), '\n') => false, //Don't parse Windows CRLF as Mac CR
					(Some('\r'), _) => true, //Mac
					(Some('\n'), _) => true, //Linux + Windows end
					(None, _) => true,
					_ => false,
				}.then_some(SourcePos::new(*ci))
			})
			.collect();

		Self {
			name,
			content,
			lines_begin_indices,
			char_to_byte,
		}
	}

	pub fn get_name(&self) -> &str {
		&self.name
	}

	pub fn get_content(&self) -> &str {
		&self.content
	}

	pub fn get_lines_begin_indices(&self) -> &[SourcePos] {
		self.lines_begin_indices.as_slice()
	}

	pub fn get_char_to_byte(&self, pos: &SourcePosMeta) -> usize {
		self.assert_safe_pos(pos);

		self.char_to_byte[pos.pos.char_index()]
	}

	pub fn chars(&self) -> CharsLen {
		CharsLen::new(self.content.chars(), self.char_to_byte.len())
	}

	pub fn get_line_index(&self, pos: &SourcePosMeta) -> usize {
		self.assert_safe_pos(pos);

		match self.lines_begin_indices.binary_search_by_key(pos, |key| key.to_meta(self)) {
			Ok(line_index) => line_index,
			Err(binary_search_left) => binary_search_left - 1,
		}
	}

	pub fn get_line(&self, pos: &SourcePosMeta) -> SourceRange {
		self.assert_safe_pos(pos);

		let line_index = self.get_line_index(pos);
		SourceRange{
			begin: self.lines_begin_indices[line_index],
			end: self.lines_begin_indices[line_index + 1],
		}
	}

	pub fn get_eof(&self) -> SourcePosMeta {
		SourcePosMeta {
			pos: SourcePos::new(self.chars().len()),
			file: &self,
		}
	}

	fn assert_safe_pos<'a>(&'a self, pos: &SourcePosMeta<'a>) {
		debug_assert!(ptr::eq(pos.file, self));
	}
}

#[cfg(test)]
mod tests {
	use super::SourceFile;

	#[test]
	fn windows_lines() {
		let src_file = SourceFile::new("windows_lines", "abc\r\nxyz\r\n\r\n123\r\n");
		assert_eq!(src_file.lines_begin_indices, vec![0, 5, 10, 12]);
	}

	#[test]
	fn mac_lines() {
		let src_file = SourceFile::new("mac_lines", "abc\rxyz\r\r123\r");
		assert_eq!(src_file.lines_begin_indices, vec![0, 4, 8, 9]);
	}

	#[test]
	fn linux_lines() {
		let src_file = SourceFile::new("linux_lines", "abc\nxyz\n\n123\n");
		assert_eq!(src_file.lines_begin_indices, vec![0, 4, 8, 9]);
	}
}
