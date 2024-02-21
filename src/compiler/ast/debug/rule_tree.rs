use crate::tree_printer::TreeDisplay;
use owo_colors::{AnsiColors, DynColors, OwoColorize};

use super::super::rule_error::ResultSHKind;

pub struct RuleTree {
	pub rule_name: &'static str,
	pub result_kind: ResultSHKind,
	pub children: Vec<RuleTree>,
}

impl TreeDisplay for RuleTree {
	fn get_text_line(&self) -> String {
		format!("Rule '{}'", self.rule_name)
			.color(self.get_scope_color())
			.to_string()
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		(self.children.len() > 0)
			.then(|| self.children
				.iter()
				.map(|child| ("".to_string(), child as &dyn TreeDisplay)) //TODO
				.collect::<Vec<_>>()) //TODO
	}

	fn get_scope_color(&self) -> DynColors {
		DynColors::Ansi(match self.result_kind {
			ResultSHKind::Success => AnsiColors::Green,
			ResultSHKind::SoftErr => AnsiColors::Yellow,
			ResultSHKind::HardErr => AnsiColors::Red,
		})
	}
}
