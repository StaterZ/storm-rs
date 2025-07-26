use super::super::node_sets::*;

#[derive(Debug)]
pub struct Call {
	pub func: Box<Node<Expr>>,
	pub arg: Box<Node<Expr>>,
}
