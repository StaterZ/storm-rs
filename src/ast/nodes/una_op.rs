use enum_display::EnumDisplay;

use super::Node;

#[derive(Debug)]
pub struct UnaOp {
	pub op: UnaOpKind,
	pub expr: Box<Node>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum UnaOpKind {
	Identity,
	Negate,
	Not,
}
