use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct For {
	pub binding: Box<Sourced<Pattern>>,
	pub iter: Box<Sourced<Expr>>,
	pub body: Box<Sourced<Expr>>,
	pub body_else: Option<Box<Sourced<Expr>>>,
}
