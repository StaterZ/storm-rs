use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct TupleCtor {
	pub items: Vec<Sourced<Expr>>,
}
