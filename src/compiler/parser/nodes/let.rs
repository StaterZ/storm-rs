use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Let {
	pub pat: Box<Sourced<Pattern>>,
}
