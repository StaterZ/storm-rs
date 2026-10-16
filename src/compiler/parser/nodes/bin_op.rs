use std::fmt::Display;
use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::node_sets::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithBinOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
}
impl Display for ArithBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ArithBinOpKind::Add => write!(f, "+"),
			ArithBinOpKind::Sub => write!(f, "-"),
			ArithBinOpKind::Mul => write!(f, "*"),
			ArithBinOpKind::Div => write!(f, "/"),
			ArithBinOpKind::Mod => write!(f, "%"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitwiseBinOpKind {
	Shl,
	Shr,
	/// What `&` becomes on bool types wider than b1.
	And,
	/// What `|` becomes on bool types wider than b1.
	Or,
	Xor,
}
impl Display for BitwiseBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BitwiseBinOpKind::Shl => write!(f, "<<"),
			BitwiseBinOpKind::Shr => write!(f, ">>"),
			BitwiseBinOpKind::And => write!(f, "&"),
			BitwiseBinOpKind::Or => write!(f, "|"),
			BitwiseBinOpKind::Xor => write!(f, "^"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicBinOpKind {
	And,
	Or,
}
impl Display for LogicBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LogicBinOpKind::And => write!(f, "&"),
			LogicBinOpKind::Or => write!(f, "|"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArithBinOp {
	pub kind: ArithBinOpKind,
	pub allow_wrap: bool,
}
impl Display for ArithBinOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.allow_wrap {
			write!(f, "%")?;
		}
		write!(f, "{}", self.kind)?;
		Ok(())
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpBinOpKind {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
}
impl Display for CmpBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CmpBinOpKind::Eq => write!(f, "=="),
			CmpBinOpKind::Ne => write!(f, "!="),
			CmpBinOpKind::Lt => write!(f, "<"),
			CmpBinOpKind::Le => write!(f, "<="),
			CmpBinOpKind::Gt => write!(f, ">"),
			CmpBinOpKind::Ge => write!(f, ">="),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
	Arith(ArithBinOp),
	Bitwise(BitwiseBinOpKind),
	Cmp(CmpBinOpKind),
	Logic(LogicBinOpKind),
}
impl Display for BinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BinOpKind::Arith(arith) => write!(f, "{}", arith),
			BinOpKind::Cmp(cmp) => write!(f, "{}", cmp),
			BinOpKind::Bitwise(bitwise) => write!(f, "{}", bitwise),
			BinOpKind::Logic(logic) => write!(f, "{}", logic),
		}
	}
}

#[derive(Debug, TreeDisplay)]
pub struct BinOp {
	pub op: BinOpKind,
	pub lhs: Box<Sourced<Expr>>,
	pub rhs: Box<Sourced<Expr>>
}
