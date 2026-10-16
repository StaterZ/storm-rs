use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, Clone, TreeDisplay)]
pub struct If {
	pub cond: Box<Sourced<Expr>>,
	pub body: Box<Sourced<Expr>>,
	#[tree(name = "else")]
	pub body_else: Option<Box<Sourced<Expr>>>,
}
