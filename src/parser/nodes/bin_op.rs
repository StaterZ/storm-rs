use std::fmt::Display;

use super::Node;

#[derive(Debug)]
pub struct BinOp {
	pub lhs: Box<Node>,
	pub op: BinOpKind,
	pub rhs: Box<Node>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, strum::AsRefStr)]
pub enum BinOpKind {
	Arith(ArithBinOpKind),
	Logic(LogicBinOpKind),
	Cmp(CmpBinOpKind),
}

impl Display for BinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}::{}", self.as_ref(), match self {
			BinOpKind::Arith(value) => value.as_ref(),
			BinOpKind::Logic(value) => value.as_ref(),
			BinOpKind::Cmp(value) => value.as_ref(),
		})
	}
}


#[derive(Debug, Clone, Copy, Eq, PartialEq, strum::AsRefStr)]
pub enum ArithBinOpKind {
	Mul,
	Div,
	Add,
	Sub,
	//Rem, //Mod,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, strum::AsRefStr)]
pub enum LogicBinOpKind {
	And,
	Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, strum::AsRefStr)]
pub enum CmpBinOpKind {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
}
