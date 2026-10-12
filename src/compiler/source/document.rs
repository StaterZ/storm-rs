use std::{iter::Map, str::CharIndices};

use super::Pos;

#[derive(Debug)]
pub struct Document {
	name: String,
	text: String,
}

impl Document {
	pub fn new(name: String, text: String) -> Self {
		Self {
			name,
			text,
		}
	}

	pub fn get_name(&self) -> &str {
		&self.name
	}

	pub fn get_text(&self) -> &str {
		&self.text
	}

	pub fn eof(&self) -> Pos {
		Pos::new(self.text.len())
	}

	pub fn char_positions(&self) -> Map<CharIndices<'_>, fn((usize, char)) -> (Pos, char)> {
		let f = |(i, c)| (Pos::new(i), c);
		self.text
			.char_indices()
			.map(f as fn((usize, char)) -> (Pos, char))
	}
}
