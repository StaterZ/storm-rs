use std::fmt::Display;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, Clone, Copy)]
pub enum UnaOpKind {
	Deref,
	AddressOf,
	Identity,
	Negate,
	Not,
}

impl Display for UnaOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			UnaOpKind::Deref => write!(f, "*"),
			UnaOpKind::AddressOf => write!(f, "&"),
			UnaOpKind::Identity => write!(f, "+"),
			UnaOpKind::Negate => write!(f, "-"),
			UnaOpKind::Not => write!(f, "!"),
		}
	}
}

#[derive(Debug)]
pub struct UnaOp {
	pub op: UnaOpKind,
	pub expr: Box<Sourced<Expr>>,
}
