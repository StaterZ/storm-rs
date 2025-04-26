use super::Node;

#[derive(Debug)]
pub struct IfElse {
	pub cond: Box<Node>,
	pub body_if: Box<Node>,
	pub body_else: Option<Box<Node>>,
}
