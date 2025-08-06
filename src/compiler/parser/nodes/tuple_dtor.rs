use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct TupleDtor {
	pub items: Vec<Sourced<Pattern>>,
}
