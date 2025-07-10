use std::rc::Rc;

use super::super::Var;

use super::{Node, Expr};

#[derive(Debug)]
pub struct FieldAccess {
	pub ident: Rc<Var>,
	pub expr: Box<Node<Expr>>,
}
