use std::fmt::Display;
use std::iter::Enumerate;

use super::Pos;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LineState {
	Other,
	LineFeed,
	CarriageReturn,
}

#[derive(Debug, Clone)]
struct State {
	pos: Pos,
	prev_line_state: LineState,
}

pub struct FilePos<I: Iterator> {
	iter: Enumerate<I>,
	state: Option<State>
}

impl<I: Iterator<Item = char>> FilePos<I> {
	pub fn new(iter: I) -> Self {
		Self {
			iter: iter.enumerate(),
			state: None,
		}
	}

	pub fn get_pos(&self) -> Option<&Pos> {
		self.state
			.as_ref()
			.map(|state| &state.pos)
	}
}

impl<I: Iterator<Item = char>> Iterator for FilePos<I> {
	type Item = I::Item;

	fn next(&mut self) -> Option<Self::Item> {
		let state = self.state.get_or_insert(State {
			pos: Pos { index: 0, line_start_index: 0, line: 1 },
			prev_line_state: LineState::Other,
		});

		self.iter.next().map(|(i, c)| {
			let line_state = match c {
				'\n' => LineState::LineFeed,
				'\r' => LineState::CarriageReturn,
				_ => LineState::Other,
			};

			let is_new_line_continued = state.prev_line_state == LineState::CarriageReturn && line_state == LineState::LineFeed;

			state.pos.index = i;
			if !is_new_line_continued && state.prev_line_state != LineState::Other {
				state.pos.line_start_index = state.pos.index;
				state.pos.line += 1;
			}

			if line_state == LineState::Other {

			}

			state.prev_line_state = line_state;

			c
		})
	}
}

impl<I: Iterator<Item = char> + Clone> Clone for FilePos<I> {
	fn clone(&self) -> Self {
		Self { 
			iter: self.iter.clone(),
			state: self.state.clone(),
		}
	}
}

impl<I: Iterator<Item = char>> Display for FilePos<I> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let state = self.state.as_ref().unwrap();
		write!(f, "{}:{}", state.pos.line, state.pos.column())
	}
}
