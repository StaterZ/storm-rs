use super::{Node, Expr};

#[derive(Debug)]
pub struct Stmt {
	pub expr: Box<Node<Expr>>,
}
