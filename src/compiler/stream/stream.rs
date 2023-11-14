use std::{iter::{Peekable, FusedIterator}, fmt::Display};
use super::{Peeker, StreamHypothetical};

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
			.get_current()
			.map_or(false, pred)
	}
	
	pub fn expect(&mut self, pred: impl FnOnce(&B) -> bool) -> Option<B> {
		self
			.check(pred)
			.then(|| self.next().unwrap())
	}
	
	pub fn expect_map<T>(&mut self, pred: impl FnOnce(&B) -> Option<T>) -> Option<(B, T)> {
		self
			.get_peeker()
			.get_current()
			.and_then(pred)
			.map(|value| (self.next().unwrap(), value))
	}

	pub fn expect_err(&mut self, pred: impl FnOnce(&B) -> Result<(), String>) -> Result<B, String> {
		match self.get_peeker().get_current() {
			Some(c) => pred(c).map(|_| self.next().unwrap()),
			None => Err("Iterator is exhausted".to_string()),
		}
	}
}

impl<I, RF, MF, B> Stream<I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
	B: Display + PartialEq,
{
	pub fn expect_eq(&mut self, expected: &B) -> Option<B> {
		self.expect(|c| c == expected)
	}

	pub fn expect_eq_err(&mut self, expected: &B) -> Result<B, String> {
		self.expect_err(|c| if c == expected {
			Ok(())
		} else {
			Err(format!("found '{}'", c))
		}).map_err(|err| format!("Expected '{}' but {}", expected, err))
	}
}

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
	
	#[inline(always)]
	pub fn hypothetically<T, E>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, E> {
		let mut hypothetical = self.dup();

		let result = f(hypothetical.get());
		hypothetical.nip_or_pop(result.is_ok());

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
