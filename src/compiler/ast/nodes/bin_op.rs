use std::fmt::Display;
use strum::AsRefStr;
use szu::ternary;

use super::Node;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
	Constant,
	Linear,
	//Exponential,
	Bitwise,
}

#[derive(Debug, PartialEq, Eq, Ord, AsRefStr)]
pub enum MathBinOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Shl,
	Shr,
}

impl MathBinOpKind {
	fn precedence(&self) -> Precedence {
		match self {
			MathBinOpKind::Add | MathBinOpKind::Sub => Precedence::Constant,
			MathBinOpKind::Mul | MathBinOpKind::Div | MathBinOpKind::Mod => Precedence::Linear,
			MathBinOpKind::Shl | MathBinOpKind::Shr => Precedence::Bitwise,
		}
	}
}

impl PartialOrd for MathBinOpKind {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.precedence().cmp(&other.precedence()))
	}
}

#[derive(Debug, PartialEq, Eq, Ord)]
pub struct MathBinOpVariant {
	pub kind: MathBinOpKind,
	pub allow_wrap: bool,
}

impl PartialOrd for MathBinOpVariant {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.kind.partial_cmp(&other.kind)
	}
}

impl Display for MathBinOpVariant {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}{}", ternary!(self.allow_wrap => "%", ""), self.kind.as_ref())
	}
}

#[derive(Debug, PartialEq, Eq, Ord)]
pub struct CmpBinOpKind {
	pub lt: bool,
	pub eq: bool,
	pub gt: bool,
}

impl PartialOrd for CmpBinOpKind {
	fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
		Some(std::cmp::Ordering::Equal)
	}
}

impl Display for CmpBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CmpBinOpKind { lt: true, eq: false, gt: false } => f.write_str("<"),
			CmpBinOpKind { lt: true, eq: true, gt: false } => f.write_str("<="),
	
			CmpBinOpKind { lt: false, eq: true, gt: false } => f.write_str("=="),
			CmpBinOpKind { lt: true, eq: false, gt: true } => f.write_str("!="),
	
			CmpBinOpKind { lt: false, eq: false, gt: true } => f.write_str(">"),
			CmpBinOpKind { lt: false, eq: true, gt: true } => f.write_str(">="),
	
			cmp => write!(f, "{:?}", cmp),
		}
	}
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpKind {
	Math(MathBinOpVariant),
	Cmp(CmpBinOpKind),
	Assign(Option<MathBinOpVariant>),
}

impl Display for BinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BinOpKind::Math(math) => f.write_str(math.kind.as_ref()),
			BinOpKind::Cmp(cmp) => f.write_str(cmp.to_string().as_str()),
			BinOpKind::Assign(assign) => f.write_str(assign.as_ref().map_or("none".to_string(), |op| op.to_string()).as_str()),
		}
	}
}

#[derive(Debug)]
pub struct BinOp {
	pub op: BinOpKind,
	pub lhs: Box<Node>,
	pub rhs: Box<Node>
}
