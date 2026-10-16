use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, Clone, TreeDisplay)]
pub struct Cast {
	pub expr: Box<Sourced<Expr>>,
	pub ty: Sourced<TypeExpr>,
}
