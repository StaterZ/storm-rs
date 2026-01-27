use super::super::node_sets::*;

#[derive(Debug)]
pub struct If {
	pub cond: Box<Expr>,
	pub body: Box<Expr>,
	pub body_else: Option<Box<Expr>>,
}
