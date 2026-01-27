use super::super::node_sets::*;

#[derive(Debug)]
pub struct Return {
	pub expr: Option<Box<Expr>>,
}
