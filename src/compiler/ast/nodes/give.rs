use super::Node;

#[derive(Debug)]
pub struct Give {
	pub expr: Box<Node>,
}
