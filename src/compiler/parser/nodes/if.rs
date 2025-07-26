use super::super::node_sets::*;

#[derive(Debug)]
pub struct If {
	pub cond: Box<Node<Expr>>,
	pub body: Box<Node<Expr>>,
	pub body_else: Option<Box<Node<Expr>>>,
}
