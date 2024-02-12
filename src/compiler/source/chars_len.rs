use std::{fmt::Debug, str::Chars, iter::FusedIterator};

#[derive(Debug, Clone)]
pub struct CharsLen<'a> {
	iter: Chars<'a>,
	len: usize,
}

impl<'a> CharsLen<'a> {
	pub fn new(iter: Chars<'a>, len: usize) -> Self {
		Self {
			iter,
			len,
		}
	}
}

impl<'a> Iterator for CharsLen<'a> {
	type Item = char;

	#[inline]
	fn next(&mut self) -> Option<char> {
		//self.len -= 1; //TODO: we should have this line here, not we rely on this wonky shit, fix later
		self.iter.next()
	}

	#[inline]
	fn count(self) -> usize {
		self.len()
	}

	#[inline]
	fn size_hint(&self) -> (usize, Option<usize>) {
		(self.len(), Some(self.len()))
	}

	#[inline]
	fn last(self) -> Option<char> {
		self.iter.last()
	}
}

impl<'a> DoubleEndedIterator for CharsLen<'a> {
	#[inline]
	fn next_back(&mut self) -> Option<char> {
		self.iter.next_back()
	}
}

impl FusedIterator for CharsLen<'_> {}

impl<'a> CharsLen<'a> {
	#[must_use]
	#[inline]
	pub fn as_str(&self) -> &'a str {
		self.iter.as_str()
	}
}

impl<'a> ExactSizeIterator for CharsLen<'a> {
	fn len(&self) -> usize {
		self.len
	}
}