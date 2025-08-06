use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Stmt {
	pub expr: Box<Sourced<Expr>>,
}
