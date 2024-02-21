use std::iter::{FusedIterator, Peekable};

use super::{
	Peeker,
	StreamErrorExpectErr,
	//StreamErrorExpectErrEq,
	StreamHypothetical,
	super::ResultSH,
};

pub struct Stream<I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	iter: Peekable<I>,
	rf: RF,
	mf: MF,
}

impl<I, RF, MF, B> Stream<I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	pub fn new(iter: I, rf: RF, mf: MF) -> Self {
		return Self{
			iter: iter.peekable(),
			rf,
			mf,
		};
	}

	pub fn get_peeker(&mut self) -> Peeker<'_, I, RF, B> {
		Peeker {
			peeked: self.iter.peek(),
			rf: &self.rf,
		}
	}

	pub fn check(&mut self, pred: impl FnOnce(&B) -> bool) -> bool {
		self
			.get_peeker()
			.get()
			.map_or(false, pred)
	}
	
	pub fn expect(&mut self, pred: impl FnOnce(&B) -> bool) -> Option<B> {
		self
			.check(pred)
			.then(|| self.next().unwrap()) //unwrap is safe here since 'check' returned true, meaning we managed to peek something
	}
	
	pub fn expect_map<T>(&mut self, pred: impl FnOnce(&B) -> Option<T>) -> Option<(B, T)> {
		self
			.get_peeker()
			.get()
			.and_then(pred)
			.map(|value| (self.next().unwrap(), value)) //unwrap is safe here since we managed to peek something
	}

	pub fn expect_err<E>(&mut self, pred: impl FnOnce(&B) -> Result<(), E>) -> Result<B, StreamErrorExpectErr<E>> {
		match self.get_peeker().get() {
			Some(item) => match pred(item) {
				Ok(_) => Ok(self.next().unwrap()), //unwrap is safe here since we managed to peek something
				Err(err) => Err(StreamErrorExpectErr::PredicateError(err)),
			},
			None => Err(StreamErrorExpectErr::StreamExhausted),
		}
	}
}

impl<I, RF, MF, B> Stream<I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
	B: PartialEq,
{
	pub fn expect_eq(&mut self, expected: &B) -> Option<B> {
		self.expect(|item| item == expected)
	}
}

// impl<I, RF, MF, B> Stream<I, RF, MF, B> where
// 	I: Iterator,
// 	RF: Fn(&I::Item) -> &B,
// 	MF: Fn(I::Item) -> B,
// 	B: Display + PartialEq,
// {
// 	pub fn expect_eq_err<'a>(&'a mut self, expected: &'a B) -> Result<B, StreamErrorExpectErrEq<&'a B>> {
// 		self.expect_err(|item| if item == expected {
// 			Ok(())
// 		} else {
// 			Err(item)
// 		}).map_err(|err| match err {
// 			StreamErrorExpectErr::StreamExhausted => StreamErrorExpectErrEq::StreamExhausted,
// 			StreamErrorExpectErr::PredicateError(found) => StreamErrorExpectErrEq::ExpectedItem { expected, found },
// 		})
// 	}
// }

impl<I, RF, MF, B> Stream<I, RF, MF, B> where
	I: Iterator + Clone,
	I::Item: Clone,
	RF: Fn(&I::Item) -> &B,
	RF: Clone,
	MF: Fn(I::Item) -> B,
	MF: Clone,
{
	pub fn dup<'a>(&mut self) -> StreamHypothetical<I, RF, MF, B> {
		StreamHypothetical::new(self)
	}
	
	pub fn try_rule<T, E>(&mut self, rule: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, E> {
		let mut hypothetical = self.dup();

		let result = rule(hypothetical.get());
		if result.is_ok() {
			hypothetical.nip();
		}

		result
	}

	pub fn try_rule_sh<T, E>(&mut self, rule: impl FnOnce(&mut Self) -> ResultSH<T, E>) -> ResultSH<T, E> {
		let mut hypothetical = self.dup();

		let result = rule(hypothetical.get());
		if matches!(result, Ok(Ok(_)) | Err(_)) {
			hypothetical.nip();
		}

		result
	}
}

impl<I, RF, MF, B> Clone for Stream<I, RF, MF, B> where
	I: Iterator + Clone,
	I::Item: Clone,
	RF: Fn(&I::Item) -> &B,
	RF: Clone,
	MF: Fn(I::Item) -> B,
	MF: Clone,
{
	fn clone(&self) -> Self {
		Self {
			iter: self.iter.clone(),
			rf: self.rf.clone(),
			mf: self.mf.clone(),
		}
	}
}

impl<I, RF, MF, B> Iterator for Stream<I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	type Item = B;

	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		self.iter
			.next()
			.map(|item| (self.mf)(item))
	}
}

impl<I, RF, MF, B> ExactSizeIterator for Stream<I, RF, MF, B>
where
	I: ExactSizeIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	fn len(&self) -> usize {
		self.iter.len()
	}
}

impl<I, RF, MF, B> DoubleEndedIterator for Stream<I, RF, MF, B>
where
	I: DoubleEndedIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	#[inline]
	fn next_back(&mut self) -> Option<Self::Item> {
		self.iter
			.next_back()
			.map(|item| (self.mf)(item))
	}

	#[inline]
	fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
		self.iter
			.nth_back(n)
			.map(|item| (self.mf)(item))
	}
}

impl<I, RF, MF, B> FusedIterator for Stream<I, RF, MF, B>
where
	I: FusedIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{ }
