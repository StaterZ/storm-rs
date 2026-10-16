use std::{cell::RefCell, fmt, rc::Rc};

use enum_as_inner::EnumAsInner;
use strum::AsRefStr;
use color_print::cformat;
use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::{super::{nodes::*, Var}, *};

#[derive(Debug, AsRefStr, EnumAsInner, TreeDisplay)]
#[tree(fmt_line = Pattern::fmt_tree_line)]
pub enum Pattern {
	Let(#[tree(flatten)] Let),
	Mut(#[tree(flatten)] Mut),
	TupleDtor(#[tree(flatten)] TupleDtor),
	Deref(#[tree(name = "expr")] Box<Sourced<Expr>>),
	Binding(#[tree(skip)] Rc<RefCell<Var>>),
	Discard,
}

impl Pattern {
	fn fmt_tree_line(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let value = match self {
			Pattern::Let(_) => None,
			Pattern::Mut(_) => None,
			Pattern::Deref(_) => None,
			Pattern::TupleDtor(_) => None,
			Pattern::Binding(value) => Some(cformat!("<cyan>{}</>", value.borrow())),
			Pattern::Discard => None,
		};
		if let Some(value) = value {
			write!(f, "{} ", value)?;
		}
		write!(f, "({})", self.as_ref())
	}
}
