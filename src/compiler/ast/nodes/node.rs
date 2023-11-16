use enum_as_inner::EnumAsInner;
use lazy_static::__Deref;
use strum::AsRefStr;
use color_print::cformat;
use szu::ternary;

use crate::tree_printer::TreeDisplay;

use super::{Block, Let, BinOp};

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum NodeKind {
	Block(Block),
	Let(Let),
	BinOp(BinOp),
	IntLit(u64),
	StrLit(String),
	Identifier(String),
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
			NodeKind::Let(_) => "".to_string(),
			NodeKind::BinOp(value) => format!("{}", value.op),
			NodeKind::IntLit(value) => cformat!("<cyan>{}</>", value),
			NodeKind::StrLit(value) => cformat!("<cyan>{:?}</>", value),
			NodeKind::Identifier(value) => cformat!("<cyan>{}</>", value),
		};
		format!("{}{}({})", text, ternary!(text.len() > 0 => " ", ""), self.kind.as_ref())
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		match &self.kind {
			NodeKind::Block(value) => Some(
				value.stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i), stmt as &dyn TreeDisplay))
					.collect()
			),
			NodeKind::Let(value) => Some(vec![
				("lhs".to_string(), value.lhs.deref()),
				("rhs".to_string(), value.rhs.as_ref().map_or(&"none", |rhs| rhs.deref())),
			]),
			NodeKind::BinOp(value) => Some(vec![
				//("kind".to_string(), &value.op.kind.as_ref()),
				/*("allowWrap".to_string(), match &value.op {
					BinOpKind::Math(math) => ternary!(math.allow_wrap => "true", "false"),
					BinOpKind::Cmp(cmp) => cmp,
				}),*/
				("lhs".to_string(), value.lhs.deref()),
				("rhs".to_string(), value.rhs.deref()),
			]),
			NodeKind::IntLit(_) => None,
			NodeKind::StrLit(_) => None,
			NodeKind::Identifier(_) => None,
		}
	}
}
