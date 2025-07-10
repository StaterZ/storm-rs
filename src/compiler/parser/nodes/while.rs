use super::{Node, Expr};

#[derive(Debug)]
pub struct While {
	pub cond: Box<Node<Expr>>,
	pub body: Box<Node<Expr>>,
	pub body_else: Option<Box<Node<Expr>>>,
}
