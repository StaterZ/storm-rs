use std::iter::Peekable;

use super::MapPeekable;

pub trait PeekableIterator : Iterator {
	fn peek(&mut self) -> Option<&Self::Item>;

	fn next_if(&mut self, pred: impl FnOnce(&Self::Item) -> bool) -> Option<Self::Item> {
		self
		.peek()
		.map_or(false, pred)
		.then(|| self.next().unwrap()) //unwrap is safe here since 'pred' returned true, meaning we managed to peek something
	}
	
	fn next_if_eq<T>(&mut self, expected: &T) -> Option<Self::Item> where Self::Item: PartialEq<T> {
		self.next_if(|item| item == expected)
	}

	fn map_peekable<RF, MF, T>(self, rf: RF, mf: MF) -> MapPeekable<Self, RF, MF, T> where
		Self: Sized,
		RF: Fn(&Self::Item) -> &T,
		MF: Fn(Self::Item) -> T,
	{
		MapPeekable::new(self, rf, mf)
	}
}

impl<I: Iterator> PeekableIterator for Peekable<I> {
	fn peek(&mut self) -> Option<&Self::Item> {
		Peekable::peek(self)
	}

	fn next_if(&mut self, func: impl FnOnce(&Self::Item) -> bool) -> Option<Self::Item> {
		Peekable::next_if(self, func)
	}

	fn next_if_eq<T>(&mut self, expected: &T) -> Option<Self::Item> where Self::Item: PartialEq<T> {
		Peekable::next_if_eq(self, expected)
	}
}
