use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use strum::AsRefStr;
use color_print::cformat;
use szu::opt_own::OptOwnStr;
use tree_printer::{TreeDisplay, TreeDisplayChild, make_list};

use super::{super::nodes::*, *};

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum Pattern {
	Let(Let),
	Mut(Mut),
	TupleDtor(TupleDtor),
	Deref(Box<Expr>),
	Binding(String),
	Discard,
}

impl TreeDisplay for Pattern {
	fn get_text_line(&self) -> String {
		let text: OptOwnStr = match self.deref() {
			Pattern::Let(_) => "".into(),
			Pattern::Mut(_) => "".into(),
			Pattern::Deref(_) => "".into(),
			Pattern::TupleDtor(_) => "".into(),
			Pattern::Binding(value) => cformat!("<cyan>{}</>", value).into(),
			Pattern::Discard => "".into(),
		};
		format!("{}{}({})", text.deref(), if text.len() > 0 { " " } else { "" }, self.deref().as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		match self.deref() {
			Pattern::Let(value) => Some(vec![
				("expr".into(), (value.pat.deref() as &dyn TreeDisplay).into()),
			]),
			Pattern::Mut(value) => Some(vec![
				("expr".into(), (value.pat.deref() as &dyn TreeDisplay).into()),
			]),
			Pattern::Deref(value) => Some(vec![
				("expr".into(), (value.deref() as &dyn TreeDisplay).into()),
			]),
			Pattern::TupleDtor(value) => Some(make_list(value.items.iter())),
			Pattern::Binding(_) => None,
			Pattern::Discard => None,
		}
	}
}
