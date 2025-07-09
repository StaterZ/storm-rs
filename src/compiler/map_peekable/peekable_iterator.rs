use std::iter::Peekable;

use super::MapPeekable;

pub trait PeekableIterator : Iterator {
	fn peek(&mut self) -> Option<&Self::Item>;

	fn map_peekable<RF, MF, B>(self, rf: RF, mf: MF) -> MapPeekable<Self, RF, MF, B> where
		Self: Sized,
		RF: Fn(&Self::Item) -> &B,
		MF: Fn(Self::Item) -> B,
	{
		MapPeekable::new(self, rf, mf)
	}
}

impl<I, RF, MF, B> PeekableIterator for MapPeekable<I, RF, MF, B> where
	I: PeekableIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	fn peek(&mut self) -> Option<&Self::Item> {
		MapPeekable::peek(self)
	}
}

impl<I: Iterator> PeekableIterator for Peekable<I> {
	fn peek(&mut self) -> Option<&Self::Item> {
		Peekable::peek(self)
	}
}
