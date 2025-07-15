use super::{
	DocumentMeta,
	Line,
	LineMeta,
};

pub struct Lines<'a> {
	document: &'a DocumentMeta<'a>,
	index: usize,
}

impl<'a> Lines<'a> {
	pub fn new(document: &'a DocumentMeta<'a>) -> Self {
		Self {
			document,
			index: 0,
		}
	}
}

impl<'a> Iterator for Lines<'a> {
	type Item = LineMeta<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		(self.index < self.document.get_num_lines()).then(|| {
			let index = self.index;
			self.index += 1;
			Line::new(index).with_meta(&self.document)
		})
	}
}
