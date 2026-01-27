use super::super::node_sets::*;

#[derive(Debug)]
pub struct Loop {
	pub body: Box<Expr>,
}
