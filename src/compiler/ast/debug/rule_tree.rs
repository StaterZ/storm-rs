use super::{
	super::{
		super::source,
		rule_error::ResultSHKind,
		Token,
	},
	RuleTreeMeta,
};

pub struct RuleTree<'a> {
	pub name: &'static str,
	pub stream_state: Option<&'a Token>,
	pub result_kind: ResultSHKind,
	pub children: Vec<RuleTree<'a>>,
}

impl<'a> RuleTree<'a> {
	pub fn with_meta<'b, 'c>(&'b self, document: &'c source::Document) -> RuleTreeMeta<'a, 'b, 'c> {
		RuleTreeMeta {
			tree: self,
			document,
		}
	}
}
