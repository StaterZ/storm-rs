use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, Clone, TreeDisplay)]
pub struct Break {
	pub expr: Option<Box<Sourced<Expr>>>,
}
