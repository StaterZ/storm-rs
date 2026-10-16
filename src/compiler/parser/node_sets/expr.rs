use std::{cell::RefCell, fmt, rc::Rc};

use enum_as_inner::EnumAsInner;
use strum::AsRefStr;
use color_print::cformat;
use tree_printer::TreeDisplay;

use crate::compiler::lexer::literal::{IntLit, RealLit};

use super::super::{nodes::*, Var};

#[derive(Debug, AsRefStr, EnumAsInner, TreeDisplay)]
#[tree(fmt_line = Expr::fmt_tree_line)]
pub enum Expr {
	Assign(#[tree(flatten)] Assign),
	Return(#[tree(flatten)] Return),
	Break(#[tree(flatten)] Break),
	Continue,
	Unreachable,

	Plex(#[tree(flatten)] Plex),

	Loop(#[tree(flatten)] Loop),
	While(#[tree(flatten)] While),
	For(#[tree(flatten)] For),
	If(#[tree(flatten)] If),

	Block(#[tree(flatten)] Block),
	Stmt(#[tree(flatten)] Stmt),
	BinOp(#[tree(flatten)] BinOp),
	UnaOp(#[tree(flatten)] UnaOp),
	FieldAccess(#[tree(flatten)] FieldAccess),
	Func(#[tree(flatten)] Func),
	Call(#[tree(flatten)] Call),
	Cast(#[tree(flatten)] Cast),
	Extern(#[tree(flatten)] Extern),
	
	TupleCtor(#[tree(flatten)] TupleCtor),
	BoolLit(#[tree(skip)] bool),
	IntLit(#[tree(skip)] IntLit),
	RealLit(#[tree(skip)] RealLit),
	StrLit(#[tree(skip)] String),
	Identifier(#[tree(skip)] Rc<RefCell<Var>>),
}

impl Expr {
	fn fmt_tree_line(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let value = match self {
			Expr::Assign(_) => None,
			Expr::Return(_) => None,
			Expr::Break(_) => None,
			Expr::Continue => None,
			Expr::Unreachable => None,

			Expr::Plex(_) => None,

			Expr::Loop(_) => None,
			Expr::While(_) => None,
			Expr::For(_) => None,
			Expr::If(_) => None,
			
			Expr::Block(_) => None,
			Expr::Stmt(_) => None,
			Expr::BinOp(_) => None,
			Expr::UnaOp(_) => None,
			Expr::FieldAccess(_) => None,
			Expr::Func(_) => None,
			Expr::Call(_) => None,
			Expr::Cast(_) => None,
			Expr::Extern(value) => Some(cformat!("<cyan>{}</>", value.binding.borrow())),

			Expr::TupleCtor(_) => None,
			Expr::BoolLit(value) => Some(cformat!("<cyan>{}</>", value)),
			Expr::IntLit(value) => Some(cformat!("<cyan>{}</>", value)),
			Expr::RealLit(value) => Some(cformat!("<cyan>{}</>", value)),
			Expr::StrLit(value) => Some(cformat!("<cyan>{:?}</>", value)),
			Expr::Identifier(value) => Some(cformat!("<cyan>{}</>", value.borrow())),
		};
		if let Some(value) = value {
			write!(f, "{} ", value)?;
		}
		write!(f, "({})", self.as_ref())
	}
}
