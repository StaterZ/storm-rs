use super::super::node_sets::*;

#[derive(Debug)]
pub struct Stmt {
	pub expr: Box<Node<Expr>>,
}
