use super::{
	Document,
	Line,
	LineMeta,
};

pub struct Lines<'a> {
	document: &'a Document,
	index: usize,
}

impl<'a> Lines<'a> {
	pub fn new(document: &'a Document) -> Self {
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
			Line::new(index).to_meta(&self.document)
		})
	}
}
