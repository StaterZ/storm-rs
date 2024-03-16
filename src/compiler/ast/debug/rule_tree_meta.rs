use crate::tree_printer::{TreeDisplay, TreeDisplayChild};
use itertools::Itertools;
use owo_colors::{AnsiColors, DynColors, OwoColorize};

use super::{
	super::{
		super::source,
		rule_error::ResultSHKind,
	},
	RuleTree,
};

pub struct RuleTreeMeta<'a, 'b, 'c> {
	pub tree: &'b RuleTree<'a>,
	pub document: &'c source::Document,
}

impl<'a, 'b, 'c> TreeDisplay for RuleTreeMeta<'a, 'b, 'c> {
	fn get_text_line(&self) -> String {
		format!("Rule '{}' Next: {}", self.tree.name, self.tree.stream_state.map_or_else(|| "EOF".to_string(), |state| format!("{}", state.with_meta(self.document))))
			.color(self.get_scope_color())
			.to_string()
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(String, TreeDisplayChild<'s>)>> {
		(self.tree.children.len() > 0)
			.then(|| self.tree.children
				.iter()
				.map(|child| ("".to_string(), TreeDisplayChild::Owned(Box::new(child.with_meta(self.document)))))
				.collect_vec()
			)
	}

	fn get_scope_color(&self) -> DynColors {
		DynColors::Ansi(match self.tree.result_kind {
			ResultSHKind::Success => AnsiColors::Green,
			ResultSHKind::SoftErr => AnsiColors::Yellow,
			ResultSHKind::HardErr => AnsiColors::Red,
		})
	}
}
