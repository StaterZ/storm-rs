use std::fmt::Display;

use super::{Node, Expr};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
	//Exponential,
	Linear,
	Bitwise,
	Constant,
	Compare,
	And,
	Or,
}

pub trait PrecedenceOrd {
	fn precedence(&self) -> Precedence;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithBinOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
}
impl PrecedenceOrd for ArithBinOpKind {
	fn precedence(&self) -> Precedence {
		match self {
			ArithBinOpKind::Add | ArithBinOpKind::Sub => Precedence::Constant,
			ArithBinOpKind::Mul | ArithBinOpKind::Div | ArithBinOpKind::Mod => Precedence::Linear,
		}
	}
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
}
impl PrecedenceOrd for BitwiseBinOpKind {
	fn precedence(&self) -> Precedence {
		Precedence::Bitwise
	}
}
impl Display for BitwiseBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BitwiseBinOpKind::Shl => write!(f, "<<"),
			BitwiseBinOpKind::Shr => write!(f, ">>"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicBinOpKind {
	And,
	Or,
}
impl PrecedenceOrd for LogicBinOpKind {
	fn precedence(&self) -> Precedence {
		match self {
			LogicBinOpKind::And => Precedence::And,
			LogicBinOpKind::Or => Precedence::Or,
		}
	}
}
impl Display for LogicBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LogicBinOpKind::And => write!(f, "&&"),
			LogicBinOpKind::Or => write!(f, "||"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArithBinOp {
	pub kind: ArithBinOpKind,
	pub allow_wrap: bool,
}
impl PrecedenceOrd for ArithBinOp {
	fn precedence(&self) -> Precedence {
		self.kind.precedence()
	}
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
impl PrecedenceOrd for CmpBinOpKind {
	fn precedence(&self) -> Precedence {
		Precedence::Compare
	}
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
impl PrecedenceOrd for BinOpKind {
	fn precedence(&self) -> Precedence {
		match self {
			BinOpKind::Arith(arith) => arith.precedence(),
			BinOpKind::Bitwise(bitwise) => bitwise.precedence(),
			BinOpKind::Cmp(cmp) => cmp.precedence(),
			BinOpKind::Logic(logic) => logic.precedence(),
		}
	}
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

#[derive(Debug)]
pub struct BinOp {
	pub op: BinOpKind,
	pub lhs: Box<Node<Expr>>,
	pub rhs: Box<Node<Expr>>
}
