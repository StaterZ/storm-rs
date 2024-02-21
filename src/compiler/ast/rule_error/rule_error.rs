use super::{
	super::super::source,
	RuleErrorKind,
	RuleErrorMeta,
};

#[derive(Debug)]
pub struct RuleError {
	pub(in super::super) kind: RuleErrorKind,
	pub(in super::super) source_range: Option<source::Range>,
}

impl RuleError {
	pub fn to_meta(self, document: &source::Document) -> RuleErrorMeta {
		RuleErrorMeta {
			error: self,
			document,
		}
	}
}
