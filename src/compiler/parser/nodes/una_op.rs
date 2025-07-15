use std::fmt::Display;

use super::{Node, Expr};

#[derive(Debug, Clone, Copy)]
pub enum UnaOpKind {
	Deref,
	AddressOf,
}

impl Display for UnaOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			UnaOpKind::Deref => write!(f, "*"),
			UnaOpKind::AddressOf => write!(f, "&"),
		}
	}
}

#[derive(Debug)]
pub struct UnaOp {
	pub op: UnaOpKind,
	pub expr: Box<Node<Expr>>,
}
