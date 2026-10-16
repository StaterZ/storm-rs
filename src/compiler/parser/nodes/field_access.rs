use std::rc::Rc;
use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::{node_sets::*, Var};

#[derive(Debug, TreeDisplay)]
pub struct FieldAccess {
	pub expr: Box<Sourced<Expr>>,
	pub ident: Rc<Var>,
}
