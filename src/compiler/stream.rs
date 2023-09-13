use std::iter::{Enumerate, Peekable};

pub struct Stream<I: Iterator> {
	iter: Peekable<Enumerate<I>>,
}

impl<I: Iterator> Stream<I> {
	pub fn new(iter: I) -> Self {
		return Self{
			iter: iter.enumerate().peekable(),
		};
	}

	pub fn get_inner(&self) -> &Peekable<Enumerate<I>> {
		&self.iter
	}

	pub fn get_current(&mut self) -> Option<&I::Item> {
		self.iter.peek().map(|(_, item)| item)
	}

	pub fn get_index(&mut self) -> Option<usize> {
		self.iter.peek().map(|(i, _)| *i)
	}

	#[inline(always)]
	pub fn check(&mut self, pred: impl FnOnce(&I::Item) -> bool) -> bool {
		self
			.get_current()
			.map_or(false, pred)
	}
	
	#[inline(always)]
	pub fn expect(&mut self, pred: impl FnOnce(&I::Item) -> bool) -> Option<I::Item> {
		if self.check(pred) {
			let result = self.next();
			assert!(result.is_some());
			result
		} else {
			None
		}
	}
	
	#[inline(always)]
	pub fn expect_map<T>(&mut self, pred: impl FnOnce(&I::Item) -> Option<T>) -> Option<(I::Item, T)> {
		self
			.get_current()
			.and_then(pred)
			.map(|value| (self.next().unwrap(), value))
	}

	#[inline(always)]
	pub fn expect_err(&mut self, pred: impl FnOnce(&I::Item) -> Result<(), String>) -> Result<I::Item, String> {
		match self.get_current() {
			Some(c) => match pred(c) {
				Ok(()) => Ok(self.next().unwrap()),
				Err(err) => Err(err),
			},
			None => Err("iterator is exhausted".to_string()),
		}
	}
}

impl<I: Iterator + Clone> Stream<I>
	where I::Item: Clone
{
	pub fn dup<'a>(&mut self) -> StreamHypothetical<I> {
		StreamHypothetical::new(self)
	}
	
	#[inline(always)]
	pub fn hypothetically<T, E>(&mut self, func: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, E> {
		let mut hypothetical = self.dup();

		let result = func(hypothetical.get());
		hypothetical.nip_or_pop(result.is_ok());

		result
	}
}

impl<I: Iterator + Clone> Clone for Stream<I>
	where I::Item: Clone
{
	fn clone(&self) -> Self {
		Self {
			iter: self.iter.clone(),
		}
	}
}

impl<I: Iterator> Iterator for Stream<I> {
	type Item = I::Item;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().map(|(_, item)| item)
	}
}



pub struct StreamHypothetical<'a, I: Iterator> {
	hypothetical: Option<Stream<I>>,
	original: &'a mut Stream<I>,
}

impl<'a, I: Iterator + Clone> StreamHypothetical<'a, I>
	where I::Item: Clone
{
	pub fn new(stream: &'a mut Stream<I>) -> Self {
		Self {
			hypothetical: Some(stream.clone()),
			original: stream,
		}
	}

	pub fn get(&mut self) -> &mut Stream<I> {
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

impl<'a, I: Iterator> Drop for StreamHypothetical<'a, I> {
	fn drop(&mut self) {
		assert!(self.hypothetical.is_none(), "stream hypothetical not collapsed");
	}
}

pub trait StreamExt : Iterator {
	fn stream(self) -> Stream<Self>
	where
		Self: Sized
	{
		Stream::new(self)
	}
}

impl<I: Iterator> StreamExt for I {}
