use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum LineState {
	Other,
	LineFeed,
	CarriageReturn,
}

#[derive(Debug, Clone)]
pub struct Pos {
	pub index: usize,
	pub line_start_index: usize,
	pub line: usize,
}

impl Pos {
	pub fn column0(&self) -> usize {
		self.index - self.line_start_index
	}

	pub fn column(&self) -> usize {
		self.column0() + 1
	}
}

impl Display for Pos {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line, self.column())
	}
}
