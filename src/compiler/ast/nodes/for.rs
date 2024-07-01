use super::Node;

#[derive(Debug)]
pub struct For {
	pub up_value: Box<Node>,
	pub iter: Box<Node>,
	pub body: Box<Node>,
}
