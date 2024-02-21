use super::Node;

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Node>,
}

impl Block {
	pub fn new() -> Self {
		Self { stmts: vec![] }
	}
}
