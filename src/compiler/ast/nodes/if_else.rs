use super::Node;

#[derive(Debug)]
pub struct IfElse {
	pub cond: Box<Node>,
	pub body_true: Box<Node>,
	pub body_false: Option<Box<Node>>,
}
