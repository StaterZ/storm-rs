use std::iter::Peekable;

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
			None => Err("iterator is exhausted".to_string()),
		}
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

pub struct Peeker<'p, I, RF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
{
	peeked: Option<&'p <Peekable<I> as Iterator>::Item>,
	rf: &'p RF,
}

impl<'p, I, RF, B> Peeker<'p, I, RF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
{
	pub fn get_current_raw(&self) -> Option<&<Peekable<I> as Iterator>::Item> {
		self.peeked.clone()
	}

	pub fn get_current(&self) -> Option<&B> {
		self
			.get_current_raw()
			.map(|item| (self.rf)(item))
	}
}

pub struct StreamHypothetical<'a, I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	hypothetical: Option<Stream<I, RF, MF, B>>,
	original: &'a mut Stream<I, RF, MF, B>,
}

impl<'a, I, RF, MF, B> StreamHypothetical<'a, I, RF, MF, B> where
	I: Iterator + Clone,
	I::Item: Clone,
	RF: Fn(&I::Item) -> &B,
	RF: Clone,
	MF: Fn(I::Item) -> B,
	MF: Clone
{
	pub fn new(stream: &'a mut Stream<I, RF, MF, B>) -> Self {
		Self {
			hypothetical: Some(stream.clone()),
			original: stream,
		}
	}

	pub fn get(&mut self) -> &mut Stream<I, RF, MF, B> {
		self.hypothetical.as_mut().unwrap()
	}

	pub fn nip(mut self) {
		*self.original = self.hypothetical.take().unwrap();
	}
	
	pub fn pop(mut self) {
		self.hypothetical = None;
	}

	pub fn nip_or_pop(self, cond: bool) {
		if cond {
			self.nip();
		} else {
			self.pop();
		}
	}
}

impl<'a, I, RF, MF, B> Drop for StreamHypothetical<'a, I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	fn drop(&mut self) {
		debug_assert!(self.hypothetical.is_none(), "stream hypothetical not collapsed");
	}
}


pub trait StreamExt : Iterator {
	fn stream<RF, MF, B>(self, rf: RF, mf: MF) -> Stream<Self, RF, MF, B> where
		Self: Sized,
		RF: Fn(&Self::Item) -> &B,
		MF: Fn(Self::Item) -> B,
	{
		Stream::new(self, rf, mf)
	}
}

impl<I: Iterator> StreamExt for I {}
