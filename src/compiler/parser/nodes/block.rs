use super::super::node_sets::*;

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Node<Expr>>,
	pub expr: Option<Box<Node<Expr>>>,
}
