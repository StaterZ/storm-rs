use super::Node;

#[derive(Debug)]
pub struct Statement {
	pub expr: Box<Node>,
}
