use std::fmt::Display;

use crate::compiler::source::SourcePos;

use super::{SourceRange, SourceFile, SourcePosMeta};

pub struct SourceRangeMeta<'a> {
	pub range: SourceRange,
	pub file: &'a SourceFile,
}

impl<'a> SourceRangeMeta<'a> {
	pub fn get_begin(&self) -> SourcePosMeta<'a> {
		self.range.begin.clone().to_meta(self.file)
	}

	pub fn get_end(&self) -> SourcePosMeta<'a> {
		self.range.end.clone().to_meta(self.file)
	}

	//TODO: move this out of here? this makes no sense here...
	pub fn get_line(&self) -> &'a str {
		let begin = self.range.begin.to_meta(self.file);
		
		let last = (self.range.end - 1).map(|last| last.to_meta(self.file));
		debug_assert_eq!(Some(begin.line_index()), last.map(|last| last.line_index()));

		begin.get_line().get_str()
	}

	pub fn get_str(&self) -> &'a str {
		&self.file.get_content()[self.range.begin.into() .. self.range.end.into()]
	}
}

impl<'a> Display for SourceRangeMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let begin = self.range.begin.to_meta(&self.file);
		let last = (self.range.end - 1)
			.unwrap_or(SourcePos::new(0))
			.to_meta(&self.file);

		debug_assert!(self.range.begin <= self.range.end);

		if self.range.begin == self.range.end {
			write!(f, "{} (0 sized)", begin)
		} else if begin.line_index() != last.line_index() {
			write!(f, "{}-{}", begin, last)
		} else if begin.pos == last.pos {
			write!(f, "{}", begin)
		} else {
			write!(f, "{}-{}", begin, last.column_number())
		}
	}
}
