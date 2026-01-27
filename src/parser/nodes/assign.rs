use super::{super::node_sets::*, BinOpKind};

#[derive(Debug)]
pub struct Assign {
	pub op: Option<BinOpKind>,
	pub lhs: Box<Pattern>,
	pub rhs: Box<Expr>,
}
