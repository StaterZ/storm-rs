use super::Node;

#[derive(Debug)]
pub struct Let {
	pub lhs: Box<Node>,
	pub rhs: Option<Box<Node>>,
}
