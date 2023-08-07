use std::{path::{Path, PathBuf}, fmt::Display};
use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;

#[derive(Debug, Clone)]
pub struct SourcePos {
	pub index: usize,
}

impl SourcePos {
	pub fn new(index: usize) -> SourcePos {
		SourcePos { index }
	}

	pub fn with_meta(&self, source: &SourceFile) -> SourcePosMeta {
		source.get_pos_meta(self.index)
	}
}

#[derive(Debug, Clone)]
pub struct SourcePosMeta {
	pub index: usize,
	pub line_start_index: usize,
	pub line: usize,
}

impl SourcePosMeta {
	pub fn column0(&self) -> usize {
		self.index - self.line_start_index
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
		debug_assert!(self.begin.index <= self.end.index);
		self.end.index - self.begin.index
	}

	pub fn get_last(&self) -> SourcePos {
		SourcePos {
			index: self.end.index - 1
		}
	}

	pub fn get_slice<'a>(&self, source: &'a str) -> &'a str {
		&source[self.begin.index .. self.end.index]
	}
	
	pub fn get_line<'a>(&self, source: &'a SourceFile) -> &'a str {
		let line_start = self.begin.with_meta(source);
		let line_end = self.end.with_meta(source);
		&source.content[line_start.line_start_index .. line_end.line_start_index]
	}
}

impl SourceRange {
	pub fn with_source<'a>(&'a self, source: &'a SourceFile) -> SourceRangeWithSource<'a> {
		SourceRangeWithSource {
			inner: self,
			source,
		}
	}
}

pub struct SourceRangeWithSource<'a> {
	pub inner: &'a SourceRange,
	pub source: &'a SourceFile,
}

impl<'a> Display for SourceRangeWithSource<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let begin = self.inner.begin.with_meta(&self.source);
		let end = self.inner.end.with_meta(&self.source);

		if begin.line == end.line {
			if begin.index == end.index {
				write!(f, "{} (0 sized)", begin)
			} else if begin.index + 1 == end.index {
				write!(f, "{}", begin)
			} else {
				write!(f, "{}-{}", begin, end.column())
			}
		} else {
			write!(f, "{}-{}", begin, end)
		}
	}
}

#[derive(Debug)]
pub struct SourceFile {
	path: PathBuf,
	content: String,
	lines_start_indices: Vec<usize>,
}

impl SourceFile {
	pub fn new(path: PathBuf, content: String) -> Self {
		let lines_start_indices = content
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
			lines_start_indices,
		}
	}

	pub fn get_path(&self) -> &Path {
		&self.path
	}

	pub fn get_content(&self) -> &str {
		&self.content
	}

	pub fn get_line_index(&self, index: usize) -> usize {
		match self.lines_start_indices.binary_search(&index) {
			Ok(line_index) => line_index,
			Err(binary_search_left) => binary_search_left - 1, //error value name is scuffed but it works
		}
	}

	pub fn get_pos_meta(&self, index: usize) -> SourcePosMeta {
		let line_index = self.get_line_index(index);
		SourcePosMeta {
			index,
			line_start_index: self.lines_start_indices[line_index],
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
		assert_eq!(src_file.lines_start_indices, vec![0, 5, 10, 12]);
	}

	#[test]
	fn mac_lines() {
		let src_file = SourceFile::new("abc\rxyz\r\r123\r".to_string());
		assert_eq!(src_file.lines_start_indices, vec![0, 4, 8, 9]);
	}

	#[test]
	fn linux_lines() {
		let src_file = SourceFile::new("abc\nxyz\n\n123\n".to_string());
		assert_eq!(src_file.lines_start_indices, vec![0, 4, 8, 9]);
	}
}
