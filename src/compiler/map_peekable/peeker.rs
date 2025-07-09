use std::iter::Peekable;

pub struct Peeker<'p, I, RF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
{
	pub(super) peeked: Option<&'p <Peekable<I> as Iterator>::Item>,
	pub(super) rf: &'p RF,
}

impl<'p, I, RF, B> Peeker<'p, I, RF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
{
	pub fn get_raw(&self) -> Option<&'p <Peekable<I> as Iterator>::Item> {
		self.peeked
	}

	#[inline]
	pub fn get(&self) -> Option<&'p B> {
		self
			.get_raw()
			.map(|item| (self.rf)(item))
	}
}
