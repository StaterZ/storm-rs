use super::{Node, Expr, Pattern};

#[derive(Debug)]
pub struct For {
	pub binding: Box<Node<Pattern>>,
	pub iter: Box<Node<Expr>>,
	pub body: Box<Node<Expr>>,
	pub body_else: Option<Box<Node<Expr>>>,
}
