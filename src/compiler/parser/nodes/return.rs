use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, TreeDisplay)]
pub struct Return {
	pub expr: Option<Box<Sourced<Expr>>>,
}
