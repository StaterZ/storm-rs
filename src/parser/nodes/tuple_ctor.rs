use super::Node;

#[derive(Debug)]
pub struct TupleCtor {
	pub items: Vec<Node>,
}

impl TupleCtor {
	pub fn new() -> Self {
		Self { items: Vec::new() }
	}
}
