use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Return {
	pub expr: Option<Box<Sourced<Expr>>>,
}
