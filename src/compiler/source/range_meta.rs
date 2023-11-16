use std::fmt::{Display, Debug};

use super::{SourceRange, SourceFile, SourcePosMeta};

#[derive(Clone, Copy)]
pub struct SourceRangeMeta<'a> {
	pub range: SourceRange,
	pub file: &'a SourceFile,
}

impl<'a> SourceRangeMeta<'a> {
	pub fn get_begin(&self) -> SourcePosMeta<'a> {
		self.range.begin.to_meta(self.file)
	}

	pub fn get_end(&self) -> SourcePosMeta<'a> {
		self.range.end.to_meta(self.file)
	}

	pub fn get_length(&self) -> usize {
		self.get_end() - self.get_begin()
	}

	pub fn get_str(&self) -> &'a str {
		&self.file.get_content()[self.range.begin.char_index() .. self.range.end.char_index()]
	}
}

impl<'a> Display for SourceRangeMeta<'a> {
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

impl<'a> Debug for SourceRangeMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.range.fmt(f)
	}
}
