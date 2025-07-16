use std::{ops::Deref, ptr};

use itertools::Itertools;
use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;

use super::super::*;

#[derive(Debug)]
pub struct DocumentMeta<'a> {
	document: &'a Document,
	pub(super) lines_begin: Vec<Pos>,
	pub(super) byte_to_char: Vec<usize>,
}

impl<'a> DocumentMeta<'a> {
	pub fn new(document: &'a Document) -> Self {
		let mut char_to_byte = Vec::<usize>::with_capacity(document.get_content().len());
		unsafe { char_to_byte.set_len(char_to_byte.capacity()); }
		let lines_begin_indices = document.get_content()
			.char_indices()
			.enumerate()
			.window_option()
			.filter_map_deref(|(prev_c, (ci, (bi, c)))| {
				char_to_byte[*bi] = *ci;
				
				let prev_c = prev_c.as_ref().map(|(_, (_, prev_c))| prev_c);
				match (prev_c, c) {
					(Some('\r'), '\n') => false, //Don't parse Windows CRLF as Mac CR
					(Some('\r'), _) => true, //Mac
					(Some('\n'), _) => true, //Linux + Windows end
					(None, _) => true,
					_ => false,
				}.then_some(Pos::new(*bi))
			})
			.collect_vec();

		Self {
			document,
			lines_begin: lines_begin_indices,
			byte_to_char: char_to_byte,
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
