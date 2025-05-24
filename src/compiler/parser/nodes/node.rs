use std::rc::Rc;
use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use strum::AsRefStr;
use color_print::cformat;
use szu::{opt_own::OptOwnStr, ternary};

use super::{
	super::{
		tree_printer::{TreeDisplay, TreeDisplayChild},
		Var,
	},
	*
};

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum NodeKind {
	Let(Let),
	Assign(Assign),
	Return(Return),
	Break(Break),
	Continue,
	Unreachable,

	Loop(Loop),
	While(While),
	For(For),
	IfElse(IfElse),

	Block(Block),
	Stmt(Stmt),
	BinOp(BinOp),
	
	TupleCtor(TupleCtor),
	IntLit(u64),
	StrLit(String),
	Identifier(Rc<Var>),
}

#[derive(Debug)]
pub struct Node {
	pub kind: NodeKind,
	//source: SourceRange,
}

impl TreeDisplay for Node {
	fn get_text_line(&self) -> String {
		let text: OptOwnStr = match &self.kind {
			NodeKind::Let(_) => "".into(),
			NodeKind::Assign(_) => "".into(),
			NodeKind::Return(_) => "".into(),
			NodeKind::Break(_) => "".into(),
			NodeKind::Continue => "".into(),
			NodeKind::Unreachable => "".into(),

			NodeKind::Loop(_) => "".into(),
			NodeKind::While(_) => "".into(),
			NodeKind::For(_) => "".into(),
			NodeKind::IfElse(_) => "".into(),
			
			NodeKind::Block(_) => "".into(),
			NodeKind::Stmt(_) => "".into(),
			NodeKind::BinOp(_) => "".into(),

			NodeKind::TupleCtor(_) => "".into(),
			NodeKind::IntLit(value) => cformat!("<cyan>{}</>", value).into(),
			NodeKind::StrLit(value) => cformat!("<cyan>{:?}</>", value).into(),
			NodeKind::Identifier(value) => cformat!("<cyan>{}</>", value).into(),
		};
		format!("{}{}({})", text.deref(), ternary!(text.len() > 0 => " ", ""), self.kind.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		match &self.kind {
			NodeKind::Let(value) => Some(vec![
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), value.rhs.as_ref().map_or(&"none" as &dyn TreeDisplay, |rhs| rhs.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Assign(value) => Some(vec![
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Return(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Break(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Continue => None,
			NodeKind::Unreachable => None,

			NodeKind::Loop(value) => Some(vec![
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::While(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::For(value) => Some(vec![
				("binding".into(), (value.binding.deref() as &dyn TreeDisplay).into()),
				("iter".into(), (value.iter.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::IfElse(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			
			NodeKind::Block(value) => Some(
				value.stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i).into(), (stmt as &dyn TreeDisplay).into()))
					.collect_vec()
			),
			NodeKind::Stmt(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::BinOp(value) => Some(vec![
				("op".into(), (&value.op as &dyn TreeDisplay).into()),
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),

			NodeKind::TupleCtor(value) => Some(
				value.items
					.iter()
					.enumerate()
					.map(|(i, item)| (format!("[{}]", i).into(), (item as &dyn TreeDisplay).into()))
					.collect_vec()
			),
			NodeKind::IntLit(_) => None,
			NodeKind::StrLit(_) => None,
			NodeKind::Identifier(_) => None,
		}
	}
}
