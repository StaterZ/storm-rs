use super::Node;

#[derive(Debug)]
pub struct For {
	pub binding: Box<Node>,
	pub iter: Box<Node>,
	pub body: Box<Node>,
	pub body_else: Option<Box<Node>>,
}
