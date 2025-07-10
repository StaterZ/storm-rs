use super::{Node, Expr};

#[derive(Debug)]
pub struct Loop {
	pub body: Box<Node<Expr>>,
	pub body_else: Option<Box<Node<Expr>>>,
}
