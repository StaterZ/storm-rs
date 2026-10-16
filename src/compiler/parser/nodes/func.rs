use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, TreeDisplay)]
pub struct Func {
	pub binding: Box<Sourced<Pattern>>,
	pub body: Box<Sourced<Expr>>,
}
