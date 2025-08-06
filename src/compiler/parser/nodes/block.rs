use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Sourced<Expr>>,
	pub expr: Option<Box<Sourced<Expr>>>,
}
