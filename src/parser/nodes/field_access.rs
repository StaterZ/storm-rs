use super::super::node_sets::*;

#[derive(Debug)]
pub struct FieldAccess {
	pub expr: Box<Expr>,
	pub ident: String,
}
