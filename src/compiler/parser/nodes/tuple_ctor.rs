use super::{Node, Expr};

#[derive(Debug)]
pub struct TupleCtor {
	pub items: Vec<Node<Expr>>,
}
