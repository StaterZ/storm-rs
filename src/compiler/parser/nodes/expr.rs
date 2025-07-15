use std::{cell::RefCell, rc::Rc};
use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use strum::AsRefStr;
use color_print::cformat;
use szu::opt_own::OptOwnStr;
use tree_printer::{make_list, TreeDisplay, TreeDisplayChild};

use super::{
	super::Var,
	*
};

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum Expr {
	Assign(Assign),
	Return(Return),
	Break(Break),
	Continue,
	Unreachable,

	Loop(Loop),
	While(While),
	For(For),
	If(If),

	Block(Block),
	Stmt(Stmt),
	BinOp(BinOp),
	UnaOp(UnaOp),
	FieldAccess(FieldAccess),
	
	TupleCtor(TupleCtor),
	IntLit(u64),
	StrLit(String),
	Identifier(Rc<RefCell<Var>>),
}

impl TreeDisplay for Node<Expr> {
	fn get_text_line(&self) -> String {
		let text: OptOwnStr = match &self.kind {
			Expr::Assign(_) => "".into(),
			Expr::Return(_) => "".into(),
			Expr::Break(_) => "".into(),
			Expr::Continue => "".into(),
			Expr::Unreachable => "".into(),

			Expr::Loop(_) => "".into(),
			Expr::While(_) => "".into(),
			Expr::For(_) => "".into(),
			Expr::If(_) => "".into(),
			
			Expr::Block(_) => "".into(),
			Expr::Stmt(_) => "".into(),
			Expr::BinOp(_) => "".into(),
			Expr::UnaOp(_) => "".into(),
			Expr::FieldAccess(_) => "".into(),

			Expr::TupleCtor(_) => "".into(),
			Expr::IntLit(value) => cformat!("<cyan>{}</>", value).into(),
			Expr::StrLit(value) => cformat!("<cyan>{:?}</>", value).into(),
			Expr::Identifier(value) => cformat!("<cyan>{}</>", value.borrow()).into(),
		};
		format!("{}{}({})", text.deref(), if text.len() > 0 { " " } else { "" }, self.kind.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		match &self.kind {
			Expr::Assign(value) => Some(vec![
				("op".into(), value.op.as_ref().map_or(&"none" as &dyn TreeDisplay, |op| op as &dyn TreeDisplay).into()),
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::Return(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::Break(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::Continue => None,
			Expr::Unreachable => None,

			Expr::Loop(value) => Some(vec![
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::While(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::For(value) => Some(vec![
				("binding".into(), (value.binding.deref() as &dyn TreeDisplay).into()),
				("iter".into(), (value.iter.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::If(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			
			Expr::Block(value) => Some(make_list(value.stmts.iter())),
			Expr::Stmt(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::BinOp(value) => Some(vec![
				("op".into(), (&value.op as &dyn TreeDisplay).into()),
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::UnaOp(value) => Some(vec![
				("op".into(), (&value.op as &dyn TreeDisplay).into()),
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			Expr::FieldAccess(value) => Some(vec![
				("ident".into(), (&value.ident as &dyn TreeDisplay).into()),
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),

			Expr::TupleCtor(value) => Some(make_list(value.items.iter())),
			Expr::IntLit(_) => None,
			Expr::StrLit(_) => None,
			Expr::Identifier(_) => None,
		}
	}
}
