use std::iter::Peekable;

use super::Stream;

pub trait PeekableIterator : Iterator {
	fn peek(&mut self) -> Option<&Self::Item>;

	fn map_peekable<RF, MF, B>(self, rf: RF, mf: MF) -> Stream<Self, RF, MF, B> where
		Self: Sized,
		RF: Fn(&Self::Item) -> &B,
		MF: Fn(Self::Item) -> B,
	{
		Stream::new(self, rf, mf)
	}
}

impl<I, RF, MF, B> PeekableIterator for Stream<I, RF, MF, B> where
	I: PeekableIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	fn peek(&mut self) -> Option<&Self::Item> {
		self.get_peeker().get()
	}
}

impl<I: Iterator> PeekableIterator for Peekable<I> {
	fn peek(&mut self) -> Option<&Self::Item> {
		Peekable::peek(self)
	}
}
