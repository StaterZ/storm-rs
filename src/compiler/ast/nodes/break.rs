use super::Node;

#[derive(Debug)]
pub struct Break {
	pub expr: Option<Box<Node>>,
}
