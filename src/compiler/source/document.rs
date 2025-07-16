use std::{iter::Map, str::CharIndices};

use super::Pos;

#[derive(Debug)]
pub struct Document {
	name: String,
	content: String,
}

impl Document {
	pub fn new(name: String, content: String) -> Self {
		Self {
			name,
			content,
		}
	}

	pub fn get_name(&self) -> &str {
		&self.name
	}

	pub fn get_content(&self) -> &str {
		&self.content
	}

	pub fn eof(&self) -> Pos {
		Pos::new(self.content.len())
	}

	pub fn char_positions(&self) -> Map<CharIndices<'_>, fn((usize, char)) -> (Pos, char)> {
		let f = |(i, c)| (Pos::new(i), c);
		self.content
			.char_indices()
			.map(f as fn((usize, char)) -> (Pos, char))
	}
}
