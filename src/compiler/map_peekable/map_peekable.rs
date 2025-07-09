use std::iter::FusedIterator;


use super::{
	PeekableIterator,
	soft_error::{SoftError, SoftResult},
};

pub enum MapPeekableExpectError<T> {
	StreamExhausted,
	PredicateError(T),
}

pub struct MapPeekable<I, RF, MF, B> where
	I: PeekableIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	iter: I,
	rf: RF,
	mf: MF,
}

impl<I, RF, MF, B> MapPeekable<I, RF, MF, B> where
	I: PeekableIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	pub fn new(iter: I, rf: RF, mf: MF) -> Self {
		return Self {
			iter,
			rf,
			mf,
		};
	}

	pub fn inner(self) -> I {
		self.iter
	}

	pub fn inner_mut(&mut self) -> &mut I {
		&mut self.iter
	}

	pub fn peek(&mut self) -> Option<&B> {
		self.iter.peek().map(&self.rf)
	}

	pub fn next_if(&mut self, pred: impl FnOnce(&B) -> bool) -> Option<B> {
		self
			.peek()
			.map_or(false, pred)
			.then(|| self.next().unwrap()) //unwrap is safe here since 'check' returned true, meaning we managed to peek something
	}
	
	pub fn next_if_map<T>(&mut self, pred: impl FnOnce(&B) -> Option<T>) -> Option<(B, T)> {
		self
			.peek()
			.and_then(pred)
			.map(|value| (self.next().unwrap(), value)) //unwrap is safe here since we managed to peek something
	}

	pub fn next_if_err<E>(&mut self, pred: impl FnOnce(&B) -> Result<(), E>) -> Result<B, MapPeekableExpectError<E>> {
		match self.peek() {
			Some(item) => match pred(item) {
				Ok(_) => Ok(self.next().unwrap()), //unwrap is safe here since we managed to peek something
				Err(err) => Err(MapPeekableExpectError::PredicateError(err)),
			},
			None => Err(MapPeekableExpectError::StreamExhausted),
		}
	}
}

impl<I, RF, MF, B> MapPeekable<I, RF, MF, B> where
	I: PeekableIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
	B: PartialEq,
{
	pub fn next_if_eq(&mut self, expected: &B) -> Option<B> {
		self.next_if(|item| item == expected)
	}
}

impl<I, RF, MF, B> MapPeekable<I, RF, MF, B> where
	I: PeekableIterator + Clone,
	I::Item: Clone,
	RF: Fn(&I::Item) -> &B,
	RF: Clone,
	MF: Fn(I::Item) -> B,
	MF: Clone,
{
	pub fn try_rule<T, E>(&mut self, rule: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, E> {
		let recover_state = self.clone();

		let result = rule(self);
		if result.is_err() {
			*self = recover_state;
		}

		result
	}

	pub fn try_rule_sh<T, ES, EH>(&mut self, rule: impl FnOnce(&mut Self) -> SoftResult<T, ES, EH>) -> SoftResult<T, ES, EH> {
		let recover_state = self.clone();

		let result = rule(self);
		if matches!(result, Err(SoftError::Soft(_))) {
			*self = recover_state;
		}

		result
	}

	pub fn try_rule_sh_arg<T, ES, EH, ARG>(&mut self, rule: impl FnOnce(&mut Self, ARG) -> SoftResult<T, ES, EH>, arg: ARG) -> SoftResult<T, ES, EH> {
		let recover_state = self.clone();

		let result = rule(self, arg);
		if matches!(result, Err(SoftError::Soft(_))) {
			*self = recover_state;
		}

		result
	}
}

impl<I, RF, MF, B> Clone for MapPeekable<I, RF, MF, B> where
	I: PeekableIterator + Clone,
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

impl<I, RF, MF, B> Iterator for MapPeekable<I, RF, MF, B> where
	I: PeekableIterator,
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

impl<I, RF, MF, B> ExactSizeIterator for MapPeekable<I, RF, MF, B>
where
	I: PeekableIterator + ExactSizeIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	fn len(&self) -> usize {
		self.iter.len()
	}
}

impl<I, RF, MF, B> DoubleEndedIterator for MapPeekable<I, RF, MF, B>
where
	I: PeekableIterator + DoubleEndedIterator,
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

impl<I, RF, MF, B> FusedIterator for MapPeekable<I, RF, MF, B>
where
	I: PeekableIterator + FusedIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{ }
