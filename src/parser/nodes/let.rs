use super::Node;

#[derive(Debug)]
pub struct Let {
	pub bind: Box<Node>,
	pub expr: Box<Node>,
}
