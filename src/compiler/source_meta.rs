use std::{path::{Path, PathBuf}, fmt::Display};
use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;
//use unicode_width::UnicodeWidthChar;

#[derive(Debug, Clone)]
pub struct SourcePos {
	pub byte_index: usize,
	pub char_index: usize,
}

impl SourcePos {
	pub fn get_prev(&self) -> Option<SourcePos> {
		(self.char_index > 0).then_some(SourcePos {
			char_index: self.char_index - 1,
			byte_index: self.byte_index - 1, //TODO: this will break with unicode
		})
	}

	pub fn to_meta(self, source: &SourceFile) -> SourcePosMeta {
		source.get_pos_meta(self)
	}
}

impl PartialEq for SourcePos {
	fn eq(&self, other: &Self) -> bool {
		let char_ok = self.char_index == other.char_index;
		let byte_ok = self.byte_index == other.byte_index;
		debug_assert_eq!(char_ok, byte_ok);
		byte_ok
	}
}


#[derive(Debug, Clone)]
pub struct SourcePosMeta {
	pub pos: SourcePos,
	pub line_begin: SourcePos,
	pub line_index: usize,
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
		write!(f, "{}:{}", self.line_index + 1, self.column())
	}
}

#[derive(Debug, Clone)]
pub struct SourceRange {
	pub begin: Option<SourcePos>,
	pub end: Option<SourcePos>,
}

impl SourceRange {
	pub fn to_workable(self) -> Option<WorkableSourceRange> {
		match (self.begin, self.end) {
			(Some(begin), Some(end)) => Some(WorkableSourceRange {
				begin,
				end,
			}),
			_ => None,
		}
	}

	pub fn with_source<'a>(&'a self, file: &'a SourceFile) -> SourceRangeWithSource<'a, Self> {
		SourceRangeWithSource {
			range: self,
			file,
		}
	}
}

#[derive(Debug, Clone)]
pub struct WorkableSourceRange {
	pub begin: SourcePos,
	pub end: SourcePos,
}

impl WorkableSourceRange {
	pub fn get_length(&self) -> usize {
		debug_assert!(self.begin.char_index <= self.end.char_index);
		self.end.char_index - self.begin.char_index
	}

	pub fn get_slice<'a>(&self, source: &'a str) -> &'a str {
		&source[self.begin.byte_index .. self.end.byte_index]
	}
	
	pub fn get_line<'a>(&self, file: &'a SourceFile) -> &'a str {
		let begin = self.begin.clone().to_meta(file);
		let last = self.end.get_prev().map(|last| last.to_meta(file));
		debug_assert_eq!(begin.line_begin, last.map_or(SourcePos {
			byte_index: 0,
			char_index: 0,
		}, |last| last.line_begin));

		let end_byte_index = if (begin.line_index + 1) < file.lines_begin_indices.len() {
			file.lines_begin_indices[begin.line_index + 1].byte_index - 1
		} else {
			file.content.len()
		};

		&file.content[begin.line_begin.byte_index .. end_byte_index]
	}
	
	pub fn with_source<'a>(&'a self, file: &'a SourceFile) -> SourceRangeWithSource<'a, Self> {
		SourceRangeWithSource {
			range: self,
			file,
		}
	}
}

pub struct SourceRangeWithSource<'a, T> {
	pub range: &'a T,
	pub file: &'a SourceFile,
}

impl<'a> Display for SourceRangeWithSource<'a, SourceRange> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let begin = self.range.begin
			.clone()
			.map(|begin| begin.to_meta(&self.file));
		let last = self.range.end
			.as_ref()
			.and_then(|end| end.get_prev())
			.map(|last| last.to_meta(&self.file));

		match (begin, last, &self.range.end) {
			(_, Some(_), None) => unreachable!(),
			(None, None, Some(_)) => unreachable!(),
			(Some(begin), None, Some(end)) => if begin.pos != *end {
				unreachable!()
			} else {
				write!(f, "{} (0 sized)", begin)
			},

			(None, _, None) => write!(f, "BOF-EOF"),
			(None, Some(last), _) => write!(f, "BOF-{}", last),
			(Some(begin), _, None) => write!(f, "{}-EOF", begin),

			(Some(begin), Some(last), _) => {
				if begin.line_index == last.line_index {
					if begin.pos.char_index == last.pos.char_index {
						write!(f, "{}", begin)
					} else {
						write!(f, "{}-{}", begin, last.column())
					}
				} else {
					write!(f, "{}-{}", begin, last)
				}
			},
		}
		
		//write!(f, "{}-EOF", begin)
	}
}
#[derive(Debug)]
pub struct SourceFile {
	path: PathBuf,
	content: String,
	lines_begin_indices: Vec<SourcePos>,
}

impl SourceFile {
	pub fn new(path: PathBuf, content: String) -> Self {
		let lines_begin_indices: Vec<SourcePos> = content
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
					Some(SourcePos{
						char_index: *i,
						byte_index: *i, //TODO: this is wrong and very scary bug nucleation site
					})
				} else {
					None
				}
			})
			.collect();

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

	pub fn get_line_index(&self, pos: &SourcePos) -> usize {
		match self.lines_begin_indices.binary_search_by_key(&pos.char_index, |pos| pos.char_index) {
			Ok(line_index) => line_index,
			Err(binary_search_left) => binary_search_left - 1,
		}
	}

	pub fn get_pos_meta(&self, pos: SourcePos) -> SourcePosMeta {
		let line_index = self.get_line_index(&pos);
		SourcePosMeta {
			pos,
			line_begin: self.lines_begin_indices[line_index].clone(),
			line_index,
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
