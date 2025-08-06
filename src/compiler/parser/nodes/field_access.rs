use std::rc::Rc;

use crate::compiler::source::Sourced;

use super::super::{node_sets::*, Var};

#[derive(Debug)]
pub struct FieldAccess {
	pub expr: Box<Sourced<Expr>>,
	pub ident: Rc<Var>,
}
