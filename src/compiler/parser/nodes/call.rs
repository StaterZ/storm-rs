use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, TreeDisplay)]
pub struct Call {
	pub func: Box<Sourced<Expr>>,
	pub arg: Box<Sourced<Expr>>,
}
