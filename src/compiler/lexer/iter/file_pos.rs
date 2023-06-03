use szu::iter::{Consumed, ConsumedExt};
use std::fmt::Display;

#[derive(Copy, Clone, PartialEq, Eq)]
enum LineState {
	Other,
	LineFeed,
	CarrageReturn,
}

#[derive(Clone)]
struct State {
	line: usize,
	line_start_index: usize,
	line_state: LineState,
}

pub struct FilePos<I: Iterator> {
	iter: Consumed<I>,
	state: Option<State>
}

impl<I: Iterator<Item = char>> FilePos<I> {
	pub fn new(iter: I) -> Self {
		Self {
			iter: iter.consumed(),
			state: None,
		}
	}

	pub fn get_line(&self) -> Option<usize> {
		self.state.as_ref().map(|state| state.line)
	}

	pub fn get_column(&self) -> Option<usize> {
		self.state.as_ref().map(|state| self.iter.get_consumed() - state.line_start_index + 1)
	}
}

impl<I: Iterator<Item = char>> Iterator for FilePos<I> {
	type Item = I::Item;

	fn next(&mut self) -> Option<Self::Item> {
		let state = self.state.get_or_insert(State {
			line: 1,
			line_start_index: 1,
			line_state: LineState::Other,
		});

		let x = self.iter.next().map(|c| {
			let is_new_line_continued = state.line_state == LineState::CarrageReturn && c == '\n';

			if !is_new_line_continued && state.line_state != LineState::Other {
				state.line_start_index = self.iter.get_consumed();
				state.line += 1;
			}

			state.line_state = match c {
				'\n' => LineState::LineFeed,
				'\r' => LineState::CarrageReturn,
				_ => LineState::Other,
			};

			c
		});

		println!("{}: {:?}", self, x);

		x
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
		write!(f, "{}:{}",
			self.get_line().unwrap_or(0),
			self.get_column().unwrap_or(0)
		)
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
