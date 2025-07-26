use super::super::node_sets::*;

#[derive(Debug)]
pub struct Func {
	pub arg: Box<Node<Expr>>,
	pub body: Box<Node<Expr>>,
}
