use super::super::node_sets::*;

#[derive(Debug)]
pub struct Func {
	pub binding: Box<Pattern>,
	pub body: Box<Expr>,
}
