use std::fmt;

use owo_colors::{AnsiColors, DynColors, OwoColorize};
use color_print::cformat;
use tree_printer::{Children, TreeDisplay};

use crate::compiler::map_peekable::soft_error::SoftError;
use crate::compiler::source;
use super::RuleTree;

pub struct RuleTreeMeta<'a, 'b, 'c> {
	pub tree: &'b RuleTree<'a>,
	pub document: &'c source::DocumentMeta<'c>,
}

impl<'a, 'b, 'c> TreeDisplay for RuleTreeMeta<'a, 'b, 'c> {
	fn fmt_line(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let line = format!("Rule '{}' Next: {}", self.tree.name, self.tree.stream_state.map_or_else(|| "EOF".to_string(), |state| {
			let range = state.source().with_meta(self.document);
			cformat!("{} <cyan>{}</>", range, range.get_str())
		}));
		write!(f, "{}", line.color(self.scope_color()))
	}

	fn fmt_children(&self, children: &mut Children<'_>) {
		for child in self.tree.children.iter() {
			children.field("", &child.with_meta(self.document));
		}
	}

	fn scope_color(&self) -> DynColors {
		DynColors::Ansi(match self.tree.result_kind {
			Ok(_) => AnsiColors::Green,
			Err(SoftError::Soft(_)) => AnsiColors::Yellow,
			Err(SoftError::Hard(_)) => AnsiColors::Red,
		})
	}
}
