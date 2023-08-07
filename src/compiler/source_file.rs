use streaming_iterator::StreamingIterator;
use szu::iter::WindowOptionExt;

#[derive(Debug)]
pub struct SourceFile {
	source: String,
	lines_start_indices: Vec<usize>,
}

impl SourceFile {
	pub fn new(source: String) -> Self {
		let lines_start_indices = source
			.chars()
			.enumerate()
			.window_option()
			.filter_map_deref(|(prev_c, (i, c))| {
				let prev_c = prev_c
					.as_ref()
					.map(|(_, prev_c)| prev_c);
				
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
			source,
			lines_start_indices,
		}
	}

	pub fn get_line(&self, index: usize) -> usize {
		match self.lines_start_indices.binary_search(&index) {
			Ok(line_index) => line_index + 1,
			Err(binary_search_left) => binary_search_left //error value name is scuffed but it works
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