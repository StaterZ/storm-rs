use super::{
	super::super::{
		RuleResult,
		TokStream,
		TokStreamIter,
		TokStreamMF,
		TokStreamRF,
	},
	RuleObserver,
};


pub struct DummyObserver {
}

impl<'i> RuleObserver<'i> for DummyObserver {
	type Signal = ();

	fn pre_rule(&mut self, _stream: &mut TokStream<'i,
		impl TokStreamIter<'i>,
		impl TokStreamRF<'i>,
		impl TokStreamMF<'i>,
	>) -> Self::Signal { }

	fn post_rule(&mut self, _rule_name: &'static str, _signal: Self::Signal, _result: &RuleResult) { }
}
