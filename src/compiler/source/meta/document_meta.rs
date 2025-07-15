use std::{ops::Deref, ptr};

use itertools::Itertools;
use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;

use super::super::*;


#[derive(Debug)]
pub struct DocumentMeta<'a> {
	pub document: &'a Document,
	lines_begin_indices: Vec<Pos>,
	char_to_byte: Vec<usize>,
}

impl<'a> DocumentMeta<'a> {
	pub fn new(document: &'a Document) -> Self {
		let mut char_to_byte = Vec::<usize>::new();
		let lines_begin_indices = document.get_content()
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
			.collect_vec();

		Self {
			document,
			lines_begin_indices,
			char_to_byte,
		}
	}
	
	pub fn get_num_lines(&self) -> usize {
		self.lines_begin_indices.len()
	}

	pub fn lines(&self) -> Lines<'_> {
		Lines::new(self)
	}

	pub(super) fn get_line_begin(&self, line: &LineMeta<'_>) -> PosMeta<'_> {
		self.assert_safe_line(line);

		self.lines_begin_indices[line.line.index()].with_meta(&self)
	}

	pub(super) fn get_char_to_byte(&self, pos: &PosMeta<'_>) -> usize {
		self.assert_safe_pos(pos);

		let char_index = pos.pos.char_index();

		if char_index == self.char_to_byte.len() { //am i the EOF char?
			self.char_to_byte[char_index - 1] + 1 //find the EOF byte
		} else {
			self.char_to_byte[char_index]
		}
	}

	pub fn get_num_chars(&self) -> usize {
		self.char_to_byte.len()
	}

	pub(super) fn get_line(&self, pos: &PosMeta<'_>) -> LineMeta<'_> {
		self.assert_safe_pos(pos);

		let line_index = match self.lines_begin_indices.binary_search_by_key(pos, |key| key.with_meta(self)) {
			Ok(line_index) => line_index,
			Err(next_line_index) => next_line_index - 1,
		};

		Line::new(line_index).with_meta(&self)
	}

	pub fn get_eof(&self) -> PosMeta<'_> {
		Pos::new(self.get_num_chars()).with_meta(&self)
	}

	fn assert_safe_line(&'a self, line: &LineMeta<'a>) {
		debug_assert!(ptr::eq(line.document, self));
	}

	fn assert_safe_pos(&'a self, pos: &PosMeta<'a>) {
		debug_assert!(ptr::eq(pos.document, self));
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
			document.lines_begin_indices
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
			document.lines_begin_indices
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
			document.lines_begin_indices
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
