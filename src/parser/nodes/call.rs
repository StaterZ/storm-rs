use super::super::node_sets::*;

#[derive(Debug)]
pub struct Call {
	pub func: Box<Expr>,
	pub arg: Box<Expr>,
}
