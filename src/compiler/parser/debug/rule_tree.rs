use crate::compiler::{map_peekable::soft_error::SoftResultUnit, source::Sourced};

use super::{
	super::{
		super::source,
		Token,
	},
	RuleTreeMeta,
};

pub struct RuleTree<'a> {
	pub name: &'static str,
	pub stream_state: Option<&'a Sourced<Token>>,
	pub result_kind: SoftResultUnit,
	pub children: Vec<RuleTree<'a>>,
}

impl<'a> RuleTree<'a> {
	pub fn with_meta<'b, 'c>(&'b self, document: &'c source::DocumentMeta<'c>) -> RuleTreeMeta<'a, 'b, 'c> {
		RuleTreeMeta {
			tree: self,
			document,
		}
	}
}
