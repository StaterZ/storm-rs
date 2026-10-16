use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, Clone, TreeDisplay)]
pub struct TupleCtor {
	#[tree(items)]
	pub items: Vec<Sourced<Expr>>,
}
