use super::{Node, Expr};

#[derive(Debug)]
pub struct Break {
	pub expr: Option<Box<Node<Expr>>>,
}
