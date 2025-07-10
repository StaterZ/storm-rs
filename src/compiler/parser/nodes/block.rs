use super::{Node, Expr};

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Node<Expr>>,
}
