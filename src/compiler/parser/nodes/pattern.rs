use std::rc::Rc;
use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use strum::AsRefStr;
use color_print::cformat;
use szu::opt_own::OptOwnStr;
use tree_printer::{TreeDisplay, TreeDisplayChild};

use super::{
	super::Var,
	*
};

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum Pattern {
	Let(Let),
	Mut(Mut),
	TupleDtor(TupleDtor),
	Deref(Box<Node<Expr>>),
	Binding(Rc<Var>),
}

impl TreeDisplay for Node<Pattern> {
	fn get_text_line(&self) -> String {
		let text: OptOwnStr = match &self.kind {
			Pattern::Let(_) => "".into(),
			Pattern::Mut(_) => "".into(),
			Pattern::Deref(_) => "".into(),
			Pattern::TupleDtor(_) => "".into(),
			Pattern::Binding(value) => cformat!("<cyan>{}</>", value).into(),
		};
		format!("{}{}({})", text.deref(), if text.len() > 0 { " " } else { "" }, self.kind.as_ref())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		fn make_list<'s, T: TreeDisplay + 's>(items: impl Iterator<Item = &'s T>) -> Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)> {
			items	
				.enumerate()
				.map(|(i, item)| (format!("[{}]", i).into(), (item as &dyn TreeDisplay).into()))
				.collect_vec()
		}

		match &self.kind {
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
		}
	}
}
