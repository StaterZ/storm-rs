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
	Block(Block),
	Statement(Statement),
	Let(Let),
	Assign(Assign),
	Return(Return),
	Give(Give),
	Break(Break),
	Continue,
	Unreachable,
	IfElse(IfElse),
	Loop(Loop),
	While(While),
	For(For),
	TupleCtor(TupleCtor),
	BinOp(BinOp),
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
		let text = match &self.kind {
			NodeKind::Block(_) => "".to_string(),
			NodeKind::Statement(_) => "".to_string(),
			NodeKind::Let(_) => "".to_string(),
			NodeKind::Assign(_) => "".to_string(),
			NodeKind::Return(_) => "".to_string(),
			NodeKind::Give(_) => "".to_string(),
			NodeKind::Break(_) => "".to_string(),
			NodeKind::Continue => "".to_string(),
			NodeKind::Unreachable => "".to_string(),
			NodeKind::IfElse(_) => "".to_string(),
			NodeKind::Loop(_) => "".to_string(),
			NodeKind::While(_) => "".to_string(),
			NodeKind::For(_) => "".to_string(),
			NodeKind::TupleCtor(_) => "".to_string(),
			NodeKind::BinOp(value) => format!("{}", value.op),
			NodeKind::IntLit(value) => cformat!("<cyan>{}</>", value),
			NodeKind::StrLit(value) => cformat!("<cyan>{:?}</>", value),
			NodeKind::Identifier(value) => cformat!("<cyan>{}</>", value),
		};
		format!("{}{}({})", text, ternary!(text.len() > 0 => " ", ""), self.kind.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		match &self.kind {
			NodeKind::Block(value) => Some(
				value.stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i).into(), (stmt as &dyn TreeDisplay).into()))
					.collect_vec()
			),
			NodeKind::Statement(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
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
			NodeKind::Give(value) => Some(vec![
				("expr".into(), (value.expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Break(value) => Some(vec![
				("expr".into(), value.expr.as_ref().map_or(&"none" as &dyn TreeDisplay, |expr| expr.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Continue => None,
			NodeKind::Unreachable => None,
			NodeKind::IfElse(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("if".into(), (value.body_true.deref() as &dyn TreeDisplay).into()),
				("else".into(), value.body_false.as_ref().map_or(&"none" as &dyn TreeDisplay, |body_false| body_false.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::Loop(value) => Some(vec![
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::While(value) => Some(vec![
				("cond".into(), (value.cond.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::For(value) => Some(vec![
				("label".into(), (value.label.deref() as &dyn TreeDisplay).into()),
				("iter".into(), (value.iter.deref() as &dyn TreeDisplay).into()),
				("body".into(), (value.body.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::TupleCtor(value) => Some(
				value.items
					.iter()
					.enumerate()
					.map(|(i, item)| (format!("[{}]", i).into(), (item as &dyn TreeDisplay).into()))
					.collect_vec()
			),
			NodeKind::BinOp(value) => Some(vec![
				//("kind".to_string(), &value.op.kind.as_ref()),
				/*("allowWrap".to_string(), match &value.op {
					BinOpKind::Math(math) => ternary!(math.allow_wrap => "true", "false"),
					BinOpKind::Cmp(cmp) => cmp,
				}),*/
				("lhs".into(), (value.lhs.deref() as &dyn TreeDisplay).into()),
				("rhs".into(), (value.rhs.deref() as &dyn TreeDisplay).into()),
			]),
			NodeKind::IntLit(_) => None,
			NodeKind::StrLit(_) => None,
			NodeKind::Identifier(_) => None,
		}
	}
}
