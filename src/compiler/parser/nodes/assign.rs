use crate::compiler::source::Sourced;

use super::{super::node_sets::*, BinOpKind};

#[derive(Debug)]
pub struct Assign {
	pub op: Option<BinOpKind>,
	pub lhs: Box<Sourced<Pattern>>,
	pub rhs: Box<Sourced<Expr>>,
}
