use super::{Node, Expr};

#[derive(Debug)]
pub struct Return {
	pub expr: Option<Box<Node<Expr>>>,
}
