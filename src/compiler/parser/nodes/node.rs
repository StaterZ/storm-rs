use std::rc::Rc;
use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use strum::AsRefStr;
use color_print::cformat;
use szu::{opt_own::OptOwnStr, ternary};
use tree_printer::{TreeDisplay, TreeDisplayChild};

use super::{
	super::Var,
	*
};

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum NodeKind {
	Let(Let),
	Mut(Mut),
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
	FieldAccess(FieldAccess),
	Deref(Box<Node>),
	AddressOf(Box<Node>),
	
	TupleCtor(TupleCtor),
	TupleDtor(TupleDtor),
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
			NodeKind::Mut(_) => "".into(),
			NodeKind::Assign(_) => "".into(),
			NodeKind::Return(_) => "".into(),
			NodeKind::Break(_) => "".into(),
			NodeKind::Continue => "".into(),
			NodeKind::Unreachable => "".into(),

			NodeKind::Loop(_) => "".into(),
			NodeKind::While(_) => "".into(),
			NodeKind::For(_) => "".into(),
			NodeKind::If(_) => "".into(),
			
			NodeKind::Block(_) => "".into(),
			NodeKind::Stmt(_) => "".into(),
			NodeKind::BinOp(_) => "".into(),
			NodeKind::FieldAccess(_) => "".into(),
			NodeKind::Deref(_) => "".into(),
			NodeKind::AddressOf(_) => "".into(),

			NodeKind::TupleCtor(_) => "".into(),
			NodeKind::TupleDtor(_) => "".into(),
			NodeKind::IntLit(value) => cformat!("<cyan>{}</>", value).into(),
			NodeKind::StrLit(value) => cformat!("<cyan>{:?}</>", value).into(),
			NodeKind::Identifier(value) => cformat!("<cyan>{}</>", value).into(),
		};
		format!("{}{}({})", text.deref(), ternary!(text.len() > 0 => " ", ""), self.kind.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		fn make_list<'s, T: TreeDisplay + 's>(items: impl Iterator<Item = &'s T>) -> Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)> {
			items	
				.enumerate()
				.map(|(i, item)| (format!("[{}]", i).into(), (item as &dyn TreeDisplay).into()))
				.collect_vec()
		}

		match &self.kind {
			NodeKind::Let(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Mut(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
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
			NodeKind::If(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			
			NodeKind::Block(value) => Some(make_list(value.stmts.iter())),
			NodeKind::Stmt(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::BinOp(value) => Some(vec![
				("op".into(), (&value.op as &dyn TreeDisplay).into()),
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::FieldAccess(value) => Some(vec![
				("ident".into(), (&value.ident as &dyn TreeDisplay).into()),
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Deref(value) => Some(vec![
				("expr".into(), (value.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::AddressOf(value) => Some(vec![
				("expr".into(), (value.deref() as &dyn TreeDisplay).into()),
			]),

			NodeKind::TupleCtor(value) => Some(make_list(value.items.iter())),
			NodeKind::TupleDtor(value) => Some(make_list(value.items.iter())),
			NodeKind::IntLit(_) => None,
			NodeKind::StrLit(_) => None,
			NodeKind::Identifier(_) => None,
		}
	}
}
