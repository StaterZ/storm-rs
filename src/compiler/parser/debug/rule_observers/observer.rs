use crate::compiler::parser::RuleResult;

use super::super::super::{
	TokStream,
	TokStreamIter,
	TokStreamMF,
	TokStreamRF
};

pub trait Observer<'i> {
	type Signal;

	fn pre_rule<'s>(&'s mut self, stream: &'s mut TokStream<'i,
		impl TokStreamIter<'i>,
		impl TokStreamRF<'i>,
		impl TokStreamMF<'i>,
	>) -> Self::Signal;
	
	fn post_rule(&mut self, rule_name: &'static str, signal: Self::Signal, result: &RuleResult);
}
