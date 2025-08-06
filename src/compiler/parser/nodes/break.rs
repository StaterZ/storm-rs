use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Break {
	pub expr: Option<Box<Sourced<Expr>>>,
}
