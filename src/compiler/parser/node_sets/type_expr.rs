use std::fmt;

use enum_as_inner::EnumAsInner;
use strum::AsRefStr;
use color_print::cformat;
use itertools::Itertools;
use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

/// A type as written in the source, like the `u8` in `x#u8`.
#[derive(Debug, AsRefStr, EnumAsInner, TreeDisplay)]
#[tree(fmt_line = TypeExpr::fmt_tree_line)]
pub enum TypeExpr {
	Named(#[tree(skip)] String),
	Tuple(#[tree(items)] Vec<Sourced<TypeExpr>>),
}

impl TypeExpr {
	fn fmt_tree_line(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeExpr::Named(name) => write!(f, "{} ", cformat!("<cyan>{}</>", name))?,
			TypeExpr::Tuple(_) => { },
		}
		write!(f, "({})", self.as_ref())
	}
}

impl fmt::Display for TypeExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeExpr::Named(name) => write!(f, "{}", name),
			TypeExpr::Tuple(items) if items.len() == 1 => write!(f, "({},)", *items[0]),
			TypeExpr::Tuple(items) => write!(f, "({})", items.iter().map(|item| item.to_string()).join(", ")),
		}
	}
}
