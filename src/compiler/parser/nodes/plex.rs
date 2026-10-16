use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, TreeDisplay)]
pub struct Plex {
	pub name: String,
	#[tree(items)]
	pub fields: Vec<Field>,
}

#[derive(Debug, TreeDisplay)]
pub struct Field {
	pub name: String,
	pub r#type: Sourced<Expr>,
}
