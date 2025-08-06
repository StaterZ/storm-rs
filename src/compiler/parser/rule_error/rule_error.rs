use crate::compiler::source::{self, Sourced};

use super::{
	super::SoftResult,
	RuleErrorKind,
	RuleErrorMeta,
};

#[derive(Debug)]
pub struct RuleError {
	pub(in super::super) kind: RuleErrorKind,
	pub(in super::super) source_range: Option<source::Range>,
}

impl RuleError {
	pub fn to_meta<'a>(self, document: &'a source::DocumentMeta<'a>) -> RuleErrorMeta<'a> {
		RuleErrorMeta {
			error: self,
			document,
		}
	}
}

pub type RuleResult<T> = SoftResult<Sourced<T>, RuleErrorKind, RuleErrorKind>;

pub enum CreateOrPass<T> {
	Create(T),
	Pass(Sourced<T>),
}
pub type RuleResultCreateOrPass<T> = SoftResult<CreateOrPass<T>, RuleErrorKind, RuleErrorKind>;
