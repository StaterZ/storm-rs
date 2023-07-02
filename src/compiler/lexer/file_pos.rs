use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum LineState {
	Other,
	LineFeed,
	CarrageReturn,
}

#[derive(Debug, Clone)]
pub struct Pos {
	pub index: usize,
	pub line_start_index: usize,
	pub line: usize,
}

impl Pos {
	pub fn column(&self) -> usize {
		self.index - self.line_start_index
	}
}

#[derive(Debug, Clone)]
pub struct FilePosItem {
	pub pos: Pos,
	pub item: char,
}

#[derive(Debug, Clone)]
struct State {
	pos: Pos,
	prev_line_state: LineState,
}

#[derive(Clone)]
pub struct FilePos<I: Iterator<Item = char>> {
	iter: I,
	state: Option<State>
}

impl<I: Iterator<Item = char>> FilePos<I> {
	pub fn new(iter: I) -> Self {
		Self {
			iter,
			state: None,
		}
	}
}

impl<I: Iterator<Item = char>> Iterator for FilePos<I> {
	type Item = FilePosItem;

	fn next(&mut self) -> Option<Self::Item> {
		let state = self.state.get_or_insert(State {
			pos: Pos { index: 0, line_start_index: 0, line: 1 },
			prev_line_state: LineState::Other,
		});

		self.iter.next().map(|c| {
			let line_state = match c {
				'\n' => LineState::LineFeed,
				'\r' => LineState::CarrageReturn,
				_ => LineState::Other,
			};

			let is_new_line_continued = state.prev_line_state == LineState::CarrageReturn && line_state == LineState::LineFeed;

			state.pos.index += 1;
			if !is_new_line_continued && state.prev_line_state != LineState::Other {
				state.pos.line_start_index = state.pos.index;
				state.pos.line += 1;
			}

			if line_state == LineState::Other {

			}

			state.prev_line_state = line_state;

			FilePosItem {
				pos: state.pos,
				item: c,
			}
		})
	}
}

impl<I: Iterator<Item = char>> Display for FilePos<I> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match &self.state {
			Some(state) => write!(f, "{}:{}", state.pos.line, state.pos.column()),
			None => write!(f, "?:?"),
		}
	}
}

pub trait FilePosExt : Iterator<Item = char> {
	fn file_pos(self) -> FilePos<Self>
	where
		Self: Sized
	{
		FilePos::new(self)
	}
}

impl<I: Iterator<Item = char>> FilePosExt for I {}

#[cfg(test)]
mod tests {
	#[test]
	fn test0() {
		let src_in = "abc\r\nxyz\r123\nahh";
		let iter = src_in
			.chars()
			.file_pos();
		for item in iter {
			println!("{:?}", item);
		}
	}
}