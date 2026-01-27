use super::super::node_sets::*;

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Expr>,
	pub expr: Option<Box<Expr>>,
}
