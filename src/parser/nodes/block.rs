use super::Node;

#[derive(Debug)]
pub struct Block {
	pub body: Vec<Node>,
	pub expr: Option<Box<Node>>,
}
