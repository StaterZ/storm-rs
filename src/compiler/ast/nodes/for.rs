use super::Node;

#[derive(Debug)]
pub struct For {
	pub label: Box<Node>,
	pub iter: Box<Node>,
	pub body: Box<Node>,
}
