use std::ops::Deref;

use super::super::{line_break::find_line_break, *};

#[derive(Debug)]
pub struct DocumentMeta<'a> {
	document: &'a Document,
	pub(super) lines_begin: Vec<Pos>,
	pub(super) byte_to_char: Vec<usize>,
}

impl<'a> DocumentMeta<'a> {
	pub fn new(document: &'a Document) -> Self {
		let text = document.get_text();
		
		let mut byte_to_char = Vec::<usize>::with_capacity(text.len() + 1);
		unsafe { byte_to_char.set_len(text.len()); }
		for (char_i, (byte_i, _)) in text.char_indices().enumerate() {
			byte_to_char[byte_i] = char_i;
		}
		byte_to_char.push(text.len());

		// a line only begins where there's a char, so a trailing line break doesn't add an empty line
		let mut lines_begin_indices = Vec::new();
		let mut line_begin = 0;
		while line_begin < text.len() {
			lines_begin_indices.push(Pos::new(line_begin));
			let Some((index, len)) = find_line_break(&text[line_begin..]) else { break };
			line_begin += index + len;
		}

		Self {
			document,
			lines_begin: lines_begin_indices,
			byte_to_char,
		}
	}
	
	pub fn eof(&self) -> PosMeta<'_> {
		self.document.eof().with_meta(self)
	}

	pub fn get_num_lines(&self) -> usize {
		self.lines_begin.len()
	}

	pub fn get_num_chars(&self) -> usize {
		self.byte_to_char.len()
	}

	pub fn lines(&self) -> Lines<'_> {
		Lines::new(self)
	}
}

impl Deref for DocumentMeta<'_> {
	type Target = Document;

	fn deref(&self) -> &Self::Target {
		&self.document
	}
}

#[cfg(test)]
mod tests {
	use itertools::Itertools;

	use crate::compiler::source::{Document, DocumentMeta, Pos};

	#[test]
	fn windows_lines() {
		let document = Document::new("windows_lines".to_string(), "abc\r\nxyz\r\n\r\n123\r\n".to_string());
		let document = DocumentMeta::new(&document);
		assert_eq!(
			document.lines_begin
				.iter()
				.map(|p| p.with_meta(&document))
				.collect_vec(),
			vec![0, 5, 10, 12]
				.iter()
				.map(|p| Pos::new(*p).with_meta(&document))
				.collect_vec(),
		);
	}

	#[test]
	fn mac_lines() {
		let document = Document::new("mac_lines".to_string(), "abc\rxyz\r\r123\r".to_string());
		let document = DocumentMeta::new(&document);
		assert_eq!(
			document.lines_begin
				.iter()
				.map(|p| p.with_meta(&document))
				.collect_vec(),
			vec![0, 4, 8, 9]
				.iter()
				.map(|p| Pos::new(*p).with_meta(&document))
				.collect_vec(),
		);
	}

	#[test]
	fn linux_lines() {
		let document = Document::new("linux_lines".to_string(), "abc\nxyz\n\n123\n".to_string());
		let document = DocumentMeta::new(&document);
		assert_eq!(
			document.lines_begin
				.iter()
				.map(|p| p.with_meta(&document))
				.collect_vec(),
			vec![0, 4, 8, 9]
				.iter()
				.map(|p| Pos::new(*p).with_meta(&document))
				.collect_vec(),
		);
	}
}
