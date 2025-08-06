use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Mut {
	pub pat: Box<Sourced<Pattern>>,
}
