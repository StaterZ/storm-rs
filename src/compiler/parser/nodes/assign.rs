use super::{Node, Expr, Pattern};

#[derive(Debug)]
pub struct Assign {
	pub lhs: Box<Node<Pattern>>,
	pub rhs: Box<Node<Expr>>,
}
