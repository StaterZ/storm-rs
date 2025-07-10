use std::iter::FusedIterator;

use super::PeekableIterator;

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

	pub fn as_inner(&mut self) -> &mut I {
		&mut self.iter
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

impl<I, RF, MF, B> PeekableIterator for MapPeekable<I, RF, MF, B> where
	I: PeekableIterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	fn peek(&mut self) -> Option<&Self::Item> {
		self.iter.peek().map(&self.rf)
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

// TODO: too unstable to use ATM, use later when it works
// unsafe impl<I, RF, MF, B> SourceIter for MapPeekable<I, RF, MF, B>
// where
// 	I: PeekableIterator,
// 	RF: Fn(&I::Item) -> &B,
// 	MF: Fn(I::Item) -> B,
// {
// 	type Source = I;

// 	unsafe fn as_inner(&mut self) -> &mut Self::Source {
// 		&mut self.iter
// 	}
// }
