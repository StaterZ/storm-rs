pub enum StreamErrorExpectErr<T> {
	StreamExhausted,
	PredicateError(T),
}

pub enum StreamErrorExpectErrEq<T> {
	StreamExhausted,
	ExpectedItem {
		expected: T,
		found: T,
	},
}
