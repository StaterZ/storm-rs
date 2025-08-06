use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Loop {
	pub body: Box<Sourced<Expr>>,
	pub body_else: Option<Box<Sourced<Expr>>>,
}
