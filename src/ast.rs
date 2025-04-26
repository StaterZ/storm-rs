use std::{fmt::Display, ops::Deref};

use color_print::cformat;
use enum_display::EnumDisplay;
use strum::AsRefStr;
use szu::{opt_own::OptOwnStr, ternary};

use crate::tree_printer::{TreeDisplay, TreeDisplayChild};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstError {
	BadNumber(std::num::ParseIntError),
}

#[derive(Debug, AsRefStr)]
pub enum Expr {
	Error,
	Lit(Lit),
	UnaOp { op: UnaOp, expr: Box<Expr> },
	BinOp { lhs: Box<Expr>, op: BinOp, rhs: Box<Expr> },
	Let { binding: String, expr: Box<Expr> },
	Block(Vec<Expr>),
}

impl TreeDisplay for Expr {
	fn get_text_line<'s>(&'s self) -> String {
		let text: OptOwnStr = match self {
			Expr::Error => cformat!("<red>Error</>").into(),
			Expr::Lit(lit) => match lit {
				Lit::Int(value) => cformat!("<cyan>{}</>", value),
				Lit::Float(value) => cformat!("<cyan>{}</>", value),
				Lit::String(value) => cformat!("<cyan>{}</>", value),
				Lit::Ident(value) => cformat!("<cyan>{}</>", value),
			}.into(),
			Expr::UnaOp { op, expr: _} => format!("{}", op).into(),
			Expr::BinOp { lhs: _, op, rhs: _ } => format!("{}", op).into(),
			Expr::Let { binding: _, expr: _ } => "".into(),
			Expr::Block(_) => "".into(),
		};
		format!("{}{}({})", text.deref(), ternary!(text.len() > 0 => " ", ""), self.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(szu::opt_own::OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		match self {
			Expr::Error => None,
			Expr::Lit(_) => None,
			Expr::UnaOp { op, expr} => Some(vec![
				("op".into(), (op as &dyn TreeDisplay).into()),
				("expr".into(), (expr.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::BinOp { lhs, op: _, rhs } => Some(vec![
				("lhs".into(), (lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (rhs.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::Let { binding, expr } => Some(vec![
				("binding".into(), (binding as &dyn TreeDisplay).into()),
				("expr".into(), (expr.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::Block(stmts) => Some(
				stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i).into(), (stmt as &dyn TreeDisplay).into()))
					.collect()
			),
		}
	}
}

#[derive(Debug)]
pub enum Lit {
	Int(i32),
	Float(f32),
	String(String),
	Ident(String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum UnaOp {
	Identity,
	Negate,
	Not,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, AsRefStr)]
pub enum BinOp {
	Arith(ArithBinOp),
	Logic(LogicBinOp),
	Cmp(CmpBinOp),
}

impl Display for BinOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}::{}", self.as_ref(), match self {
			BinOp::Arith(value) => value.to_string(),
			BinOp::Logic(value) => value.to_string(),
			BinOp::Cmp(value) => value.to_string(),
		})
	}
}


#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum ArithBinOp {
	Mul,
	Div,
	Add,
	Sub,
	//Rem,
	//Mod,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum LogicBinOp {
	And,
	Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, EnumDisplay)]
pub enum CmpBinOp {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
}
