use std::fmt;

use enum_as_inner::EnumAsInner;
use strum::AsRefStr;
use color_print::cformat;
use itertools::Itertools;
use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

/// A type as written in the source, like the `u8` in `x#u8`.
#[derive(Debug, Clone, AsRefStr, EnumAsInner, TreeDisplay)]
#[tree(fmt_line = TypeExpr::fmt_tree_line)]
pub enum TypeExpr {
	Named(#[tree(skip)] String),
	Tuple(#[tree(items)] Vec<Sourced<TypeExpr>>),
	Func(#[tree(flatten)] FuncTypeExpr),
}

/// `fn(T, U) -> R`. A trailing `...` takes any number of extra arguments, like C's `printf`.
#[derive(Debug, Clone, TreeDisplay)]
pub struct FuncTypeExpr {
	#[tree(items)]
	pub args: Vec<Sourced<TypeExpr>>,
	#[tree(skip)]
	pub variadic: bool,
	pub ret: Option<Box<Sourced<TypeExpr>>>,
}

impl TypeExpr {
	fn fmt_tree_line(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeExpr::Named(name) => write!(f, "{} ", cformat!("<cyan>{}</>", name))?,
			TypeExpr::Tuple(_) | TypeExpr::Func(_) => { },
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
			TypeExpr::Func(func) => {
				let variadic = func.variadic.then_some("...".to_string());
				write!(f, "fn({})", func.args.iter().map(|arg| arg.to_string()).chain(variadic).join(", "))?;
				if let Some(ret) = &func.ret {
					write!(f, " -> {}", ***ret)?;
				}
				Ok(())
			},
		}
	}
}
