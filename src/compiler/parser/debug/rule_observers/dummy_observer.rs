use crate::compiler::parser::TokStream;

use super::{
	super::super::RuleResult,
	Observer,
};


pub struct DummyObserver {
}

impl<'i> Observer<'i> for DummyObserver {
	type Signal = ();

	fn pre_rule(&mut self, _stream: &mut impl TokStream<'i>) -> Self::Signal { }

	fn post_rule<T>(&mut self, _rule_name: &'static str, _signal: Self::Signal, _result: &RuleResult<T>) { }
}
