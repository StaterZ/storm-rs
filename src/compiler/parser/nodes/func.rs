use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Func {
	pub binding: Box<Sourced<Pattern>>,
	pub body: Box<Sourced<Expr>>,
}
