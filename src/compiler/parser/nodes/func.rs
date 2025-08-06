use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Func {
	pub arg: Box<Sourced<Expr>>,
	pub body: Box<Sourced<Expr>>,
}
