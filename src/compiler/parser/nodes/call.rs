use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Call {
	pub func: Box<Sourced<Expr>>,
	pub arg: Box<Sourced<Expr>>,
}
