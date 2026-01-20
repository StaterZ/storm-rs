use super::Node;

#[derive(Debug)]
pub struct UnaOp {
	pub op: UnaOpKind,
	pub expr: Box<Node>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, strum::AsRefStr)]
pub enum UnaOpKind {
	Identity,
	Negate,
	Invert,
}
