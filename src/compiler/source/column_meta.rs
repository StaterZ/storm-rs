#[derive(Debug, Clone)]
pub struct ColumnMeta<'a> {
	column: Column,
	file: &'a SourceFile,
}

impl<'a> Display for ColumnMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.column.is_eof() {
			write!(f, "EOF")
		} else {
			write!(f, "{}", self.column_number())
		}
	}
}
