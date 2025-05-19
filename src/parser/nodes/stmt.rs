use super::Node;

#[derive(Debug)]
pub struct Stmt {
	pub expr: Box<Node>,
}
