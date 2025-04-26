use std::fmt::Display;

use enum_display::EnumDisplay;
use strum::AsRefStr;

use super::Node;

#[derive(Debug)]
pub struct BinOp {
	pub lhs: Box<Node>,
	pub op: BinOpKind,
	pub rhs: Box<Node>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, AsRefStr)]
pub enum BinOpKind {
	Arith(ArithBinOpKind),
	Logic(LogicBinOpKind),
	Cmp(CmpBinOpKind),
}

impl Display for BinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}::{}", self.as_ref(), match self {
			BinOpKind::Arith(value) => value.to_string(),
			BinOpKind::Logic(value) => value.to_string(),
			BinOpKind::Cmp(value) => value.to_string(),
		})
	}
}


#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum ArithBinOpKind {
	Mul,
	Div,
	Add,
	Sub,
	//Rem,
	//Mod,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum LogicBinOpKind {
	And,
	Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum CmpBinOpKind {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
}
