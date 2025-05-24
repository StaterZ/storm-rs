use super::Node;

#[derive(Debug)]
pub struct Assign {
	pub lhs: Box<Node>,
	pub rhs: Box<Node>,
}
