use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, TreeDisplay)]
pub struct Block {
	#[tree(items)]
	pub stmts: Vec<Sourced<Expr>>,
	pub expr: Option<Box<Sourced<Expr>>>,
}
