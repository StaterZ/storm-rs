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
	pub fn get_current_raw(&self) -> Option<&<Peekable<I> as Iterator>::Item> {
		self.peeked.clone() //TODO: why clone here? don't remember, seems dumb...
	}

    #[inline]
	pub fn get_current(&self) -> Option<&B> {
		self
			.get_current_raw()
			.map(|item| (self.rf)(item))
	}
}
