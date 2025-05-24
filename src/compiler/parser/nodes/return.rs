use super::Node;

#[derive(Debug)]
pub struct Return {
	pub expr: Option<Box<Node>>,
}
