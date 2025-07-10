use crate::compiler::parser::{RuleResult, TokStream};

pub trait Observer<'i> {
	type Signal;

	fn pre_rule<'s>(&'s mut self, stream: &'s mut impl TokStream<'i>) -> Self::Signal;
	
	fn post_rule<T>(&mut self, rule_name: &'static str, signal: Self::Signal, result: &RuleResult<T>);
}
