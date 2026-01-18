use std::debug_assert_eq;

use crate::compiler::{parser::{RuleResult, SoftError}, source::Sourced};

use super::super::{
	super::{
		super::lexer::Token,
		RuleObserver,
		TokStream,
	},
	RuleTree,
};

pub struct DebugSignal<'i> {
	stream_state: Option<&'i Sourced<Token>>,
	rule_tree_parent_children: Vec<RuleTree<'i>>,
}

pub struct DebugObserver<'i> {
	tree: Vec<RuleTree<'i>>,
}

impl<'i> DebugObserver<'i> {
	pub fn new() -> Self {
		Self {
			tree: Vec::<RuleTree<'i>>::new(),
		}
	}

	pub fn conclude(mut self) -> RuleTree<'i> {
		debug_assert_eq!(self.tree.len(), 1);
		self.tree.swap_remove(0)
	}
}

impl<'i> RuleObserver<'i> for DebugObserver<'i> {
	type Signal = DebugSignal<'i>;

	fn pre_rule<'s>(&'s mut self, stream: &'s mut impl TokStream<'i>) -> Self::Signal {
		Self::Signal {
			stream_state: stream.peek().map(|state| *state),
			rule_tree_parent_children: std::mem::replace(&mut self.tree, vec![]),
		}
	}

	fn post_rule<T>(&mut self, rule_name: &'static str, signal: Self::Signal, result: &RuleResult<T>) {
		let children = std::mem::replace(&mut self.tree, signal.rule_tree_parent_children);
		self.tree.push(RuleTree {
			name: rule_name,
			stream_state: signal.stream_state,
			result_kind: match result {
				Ok(_) => Ok(()),
				Err(SoftError::Soft(_)) => Err(SoftError::Soft(())),
				Err(SoftError::Hard(_)) => Err(SoftError::Hard(())),
			},
			children,
		});
	}
}
