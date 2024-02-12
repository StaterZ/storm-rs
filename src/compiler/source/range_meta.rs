use std::fmt::{Display, Debug};

use super::{Document, Range, PosMeta};

#[derive(Clone, Copy)]
pub struct RangeMeta<'a> {
	pub range: Range,
	pub document: &'a Document,
}

impl<'a> RangeMeta<'a> {
	pub fn get_begin(&self) -> PosMeta<'a> {
		self.range.begin.to_meta(self.document)
	}

	pub fn get_end(&self) -> PosMeta<'a> {
		self.range.end.to_meta(self.document)
	}

	pub fn get_length(&self) -> usize {
		self.get_end() - self.get_begin()
	}

	pub fn get_str(&self) -> &'a str {
		&self.document.get_content()[self.range.begin.char_index() .. self.range.end.char_index()]
	}
}

impl<'a> Display for RangeMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let begin = self.get_begin();
		let end = self.get_end();

		if begin == end {
			write!(f, "{} (0 sized)", begin)
		} else {
			let last = end - 1;

			if begin.line() != last.line() {
				write!(f, "{}-{}", begin, last)
			} else if begin == last {
				write!(f, "{}", begin)
			} else {
				write!(f, "{}-{}", begin, last.column().unwrap())
			}
		}
	}
}

impl<'a> Debug for RangeMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.range.fmt(f)
	}
}
