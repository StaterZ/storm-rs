use std::ptr;

use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;

use super::{
	Pos,
	PosMeta,
	Range,
};

#[derive(Debug)]
pub struct Document {
	name: String,
	content: String,
	lines_begin_indices: Vec<Pos>,
	char_to_byte: Vec<usize>,
}

impl Document {
	pub fn new(name: String, content: String) -> Self {
		let mut char_to_byte = Vec::<usize>::new();
		let lines_begin_indices: Vec<Pos> = content
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
				}.then_some(Pos::new(*ci))
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

	pub fn get_lines_begin_indices(&self) -> &[Pos] {
		self.lines_begin_indices.as_slice()
	}

	pub fn get_char_to_byte(&self, pos: &PosMeta) -> usize {
		self.assert_safe_pos(pos);

		self.char_to_byte[pos.pos.char_index()]
	}

	pub fn chars_len(&self) -> usize {
		self.char_to_byte.len()
	}

	pub fn get_line_index(&self, pos: &PosMeta) -> usize {
		self.assert_safe_pos(pos);

		match self.lines_begin_indices.binary_search_by_key(pos, |key| key.to_meta(self)) {
			Ok(line_index) => line_index,
			Err(binary_search_left) => binary_search_left - 1,
		}
	}

	pub fn get_line(&self, pos: &PosMeta) -> Range {
		self.assert_safe_pos(pos);

		let line_index = self.get_line_index(pos);
		Range{
			begin: self.lines_begin_indices[line_index],
			end: self.lines_begin_indices[line_index + 1],
		}
	}

	pub fn get_eof(&self) -> PosMeta {
		Pos::new(self.chars_len()).to_meta(&self)
	}

	fn assert_safe_pos<'a>(&'a self, pos: &PosMeta<'a>) {
		debug_assert!(ptr::eq(pos.document, self));
	}
}

#[cfg(test)]
mod tests {
	use super::{Document, Pos};

	#[test]
	fn windows_lines() {
		let src_file = Document::new("windows_lines".to_string(), "abc\r\nxyz\r\n\r\n123\r\n".to_string());
		assert_eq!(
			src_file.lines_begin_indices
				.iter()
				.map(|p| p.to_meta(&src_file))
				.collect::<Vec<_>>(),
			vec![0, 5, 10, 12]
				.iter()
				.map(|p| Pos::new(*p).to_meta(&src_file))
				.collect::<Vec<_>>(),
		);
	}

	#[test]
		fn mac_lines() {
		let src_file = Document::new("mac_lines".to_string(), "abc\rxyz\r\r123\r".to_string());
		assert_eq!(
			src_file.lines_begin_indices
				.iter()
				.map(|p| p.to_meta(&src_file))
				.collect::<Vec<_>>(),
			vec![0, 4, 8, 9]
				.iter()
				.map(|p| Pos::new(*p).to_meta(&src_file))
				.collect::<Vec<_>>(),
		);
	}

	#[test]
	fn linux_lines() {
		let src_file = Document::new("linux_lines".to_string(), "abc\nxyz\n\n123\n".to_string());
		assert_eq!(
			src_file.lines_begin_indices
				.iter()
				.map(|p| p.to_meta(&src_file))
				.collect::<Vec<_>>(),
			vec![0, 4, 8, 9]
				.iter()
				.map(|p| Pos::new(*p).to_meta(&src_file))
				.collect::<Vec<_>>(),
		);
	}
}
