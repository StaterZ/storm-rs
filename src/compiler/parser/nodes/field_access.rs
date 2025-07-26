use std::rc::Rc;

use super::super::Var;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct FieldAccess {
	pub expr: Box<Node<Expr>>,
	pub ident: Rc<Var>,
}
