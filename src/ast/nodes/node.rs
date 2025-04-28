use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use strum::AsRefStr;
use color_print::cformat;
use szu::{opt_own::OptOwnStr, ternary};

use crate::tree_printer::{TreeDisplay, TreeDisplayChild};
use super::*;

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum Node {
	Error,
	Block(Block),
	Stmt(Stmt),
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
	BinOp(BinOp),
	UnaOp(UnaOp),
	Literal(Literal),
}

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum Literal {
	Integer(u64),
	String(String),
	Identifier(String),
	TupleCtor(TupleCtor),
}

impl TreeDisplay for Node {
	fn get_text_line(&self) -> String {
		let text: OptOwnStr = match self {
			Node::Error => "ERROR".into(),
			Node::Block(_) => "".into(),
			Node::Stmt(_) => "".into(),
			Node::Let(_) => "".into(),
			Node::Assign(_) => "".into(),
			Node::Return(_) => "".into(),
			Node::Break(_) => "".into(),
			Node::Continue => "".into(),
			Node::Unreachable => "".into(),
			Node::IfElse(_) => "".into(),
			Node::Loop(_) => "".into(),
			Node::While(_) => "".into(),
			Node::For(_) => "".into(),
			Node::BinOp(value) => format!("{}", value.op).into(),
			Node::UnaOp(value) => format!("{}", value.op).into(),
			Node::Literal(value) => match value {
				Literal::Integer(value) => cformat!("<cyan>{}</>", value).into(),
				Literal::String(value) => cformat!("<cyan>{:?}</>", value).into(),
				Literal::Identifier(value) => cformat!("<cyan>{}</>", value).into(),
				Literal::TupleCtor(_) => "".into(),
			},
		};
		format!("{}{}({})", text.deref(), ternary!(text.len() > 0 => " ", ""), self.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		match self {
			Node::Error => None,
			Node::Block(value) => Some(
				value.stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i).into(), (stmt as &dyn TreeDisplay).into()))
					.collect_vec()
			),
			Node::Stmt(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Let(value) => Some(vec![
				("bind".into(), (value.bind.deref() as &dyn TreeDisplay).into()),
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Assign(value) => Some(vec![
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Return(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Break(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Continue => None,
			Node::Unreachable => None,
			Node::IfElse(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("if".into(), (value.body_if.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_else.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_else| body_else.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Loop(value) => Some(vec![
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
			]),
			Node::While(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
			]),
			Node::For(value) => Some(vec![
				("bind".into(), (value.bind.deref() as &dyn TreeDisplay).into()),
				("iter".into(), (value.iter.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
			]),
			Node::BinOp(value) => Some(vec![
				/*("allowWrap".to_string(), match &value.op {
					BinOpKind::Math(math) => ternary!(math.allow_wrap => "true", "false"),
					BinOpKind::Cmp(cmp) => cmp,
				}),*/
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			Node::UnaOp(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			Node::Literal(value) => match value {
				Literal::Integer(_) => None,
				Literal::String(_) => None,
				Literal::Identifier(_) => None,
				Literal::TupleCtor(value) => Some(
					value.items
						.iter()
						.enumerate()
						.map(|(i, item)| (format!("[{}]", i).into(), (item as &dyn TreeDisplay).into()))
						.collect_vec()
				),
			},
		}
	}
}
