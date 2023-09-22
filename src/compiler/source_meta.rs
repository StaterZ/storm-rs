use std::{path::{Path, PathBuf}, fmt::Display};
use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;
use unicode_width::UnicodeWidthChar;

#[derive(Debug, Clone)]
pub struct SourcePos {
	pub byte_index: usize,
	pub char_index: usize,
}

impl SourcePos {
	pub fn with_meta(self, source: &SourceFile) -> SourcePosMeta {
		source.get_pos_meta(self)
	}
}

impl PartialEq for SourcePos {
	fn eq(&self, other: &Self) -> bool {
		let char_ok = self.char_index == other.char_index;
		let byte_ok = self.byte_index == other.byte_index;
		assert!(char_ok == byte_ok);
		byte_ok
	}
}


#[derive(Debug, Clone)]
pub struct SourcePosMeta {
	pub pos: SourcePos,
	pub line_begin: SourcePos,
	pub line: usize,
}

impl SourcePosMeta {
	pub fn column0(&self) -> usize {
		self.pos.char_index - self.line_begin.char_index
	}

	pub fn column(&self) -> usize {
		self.column0() + 1
	}
}

impl Display for SourcePosMeta {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line, self.column())
	}
}

#[derive(Debug, Clone)]
pub struct SourceRange {
	pub begin: SourcePos,
	pub end: SourcePos,
}

impl SourceRange {
	pub fn get_length(&self) -> usize {
		debug_assert!(self.begin.char_index <= self.end.char_index);
		self.begin.char_index - self.end.char_index
	}

	pub fn get_last(&self) -> Option<SourcePos> {
		if self.end.char_index == 0 {
			None
		} else {
			Some(SourcePos {
				char_index: self.end.char_index - 1,
				byte_index: 666,
			})
		}
	}

	pub fn get_slice<'a>(&self, source: &'a str) -> &'a str {
		&source[self.begin.byte_index .. self.end.byte_index]
	}
	
	pub fn get_line<'a>(&self, source: &'a SourceFile) -> Option<&'a str> {
		let begin = self.begin.clone().with_meta(source);
		let last = self.get_last()?.with_meta(source);
		assert_eq!(begin.line_begin, last.line_begin);

		let end_byte_index = if begin.line < source.lines_begin_indices.len() {
			source.lines_begin_indices[begin.line] - 1
		} else {
			source.content.len()
		};
		Some(&source.content[begin.line_begin.byte_index .. end_byte_index])
	}
	
	pub fn with_source<'a>(&'a self, file: &'a SourceFile) -> SourceRangeWithSource<'a> {
		SourceRangeWithSource {
			range: self,
			file,
		}
	}
}

pub struct SourceRangeWithSource<'a> {
	pub range: &'a SourceRange,
	pub file: &'a SourceFile,
}

impl<'a> Display for SourceRangeWithSource<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let begin = self.range.begin.clone().with_meta(&self.file);
		let last = self.range.get_last().map(|last| last.with_meta(&self.file));

		match last {
			Some(last) if self.range.begin != self.range.end => {
				if begin.line == last.line {
					if begin.pos.char_index == last.pos.char_index {
						write!(f, "{}", begin)
					} else {
						write!(f, "{}-{}", begin, last.column())
					}
				} else {
					write!(f, "{}-{}", begin, last)
				}
			},
			_ => write!(f, "{} (0 sized)", begin),
		}
		
		//write!(f, "{}-EOF", begin)
	}
}

#[derive(Debug)]
pub struct SourceFile {
	path: PathBuf,
	content: String,
	lines_begin_indices: Vec<usize>,
}

impl SourceFile {
	pub fn new(path: PathBuf, content: String) -> Self {
		let lines_begin_indices = content
			.chars()
			.enumerate()
			.window_option()
			.filter_map_deref(|(prev_c, (i, c))| {
				let prev_c = prev_c.as_ref().map(|(_, prev_c)| prev_c);

				if match (prev_c, c) {
					(Some('\r'), '\n') => false,
					(Some('\r'), _) => true,
					(Some('\n'), _) => true,
					(None, _) => true,
					_ => false,
				} {
					Some(*i)
				} else {
					None
				}
			})
			.collect::<Vec<usize>>();

		Self {
			path,
			content,
			lines_begin_indices,
		}
	}

	pub fn get_path(&self) -> &Path {
		&self.path
	}

	pub fn get_content(&self) -> &str {
		&self.content
	}

	pub fn get_line_index(&self, char_index: usize) -> usize {
		match self.lines_begin_indices.binary_search(&char_index) {
			Ok(line_index) => line_index,
			Err(binary_search_left) => binary_search_left - 1, //error value name is scuffed but it works
		}
	}

	pub fn get_pos_meta(&self, pos: SourcePos) -> SourcePosMeta {
		let line_index = self.get_line_index(pos.char_index);
		SourcePosMeta {
			pos,
			line_begin: SourcePos {
				char_index: self.lines_begin_indices[line_index],
				byte_index: 666,
			},
			line: line_index + 1,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::SourceFile;

	#[test]
	fn windows_lines() {
		let src_file = SourceFile::new("abc\r\nxyz\r\n\r\n123\r\n".to_string());
		assert_eq!(src_file.lines_begin_indices, vec![0, 5, 10, 12]);
	}

	#[test]
	fn mac_lines() {
		let src_file = SourceFile::new("abc\rxyz\r\r123\r".to_string());
		assert_eq!(src_file.lines_begin_indices, vec![0, 4, 8, 9]);
	}

	#[test]
	fn linux_lines() {
		let src_file = SourceFile::new("abc\nxyz\n\n123\n".to_string());
		assert_eq!(src_file.lines_begin_indices, vec![0, 4, 8, 9]);
	}
}
