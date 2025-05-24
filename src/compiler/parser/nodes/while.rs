use super::Node;

#[derive(Debug)]
pub struct While {
	pub cond: Box<Node>,
	pub body: Box<Node>,
	pub body_else: Option<Box<Node>>,
}
