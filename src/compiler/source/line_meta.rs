use super::{SourceRange, SourceRangeMeta, Line, SourceFile, SourcePos};

pub struct LineMeta<'a> {
	pub line: Line,
	pub file: &'a SourceFile,
}

impl<'a> LineMeta<'a> {
	pub fn range(&self) -> SourceRangeMeta<'a> {
		let next_line = self.line.index() + 1;

		let lines_begin_indices = self.file.get_lines_begin_indices();
		let begin = lines_begin_indices[self.line.index()];
		let end = if next_line < lines_begin_indices.len() {
			lines_begin_indices[next_line]
		} else {
			SourcePos::new(self.file.get_num_chars())
		};
		
		SourceRange{
			begin,
			end,
		}.to_meta(self.file)
	}
}

impl<'a> PartialEq for LineMeta<'a> {
    fn eq(&self, other: &Self) -> bool {
		debug_assert!(self.file == other.file);
        self.line == other.line
    }
}
