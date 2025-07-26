use super::{super::node_sets::*, BinOpKind};

#[derive(Debug)]
pub struct Assign {
	pub op: Option<BinOpKind>,
	pub lhs: Box<Node<Pattern>>,
	pub rhs: Box<Node<Expr>>,
}
