use super::Stream;

pub struct StreamHypothetical<'a, I, RF, MF, B> where
	I: Iterator,
	RF: Fn(&I::Item) -> &B,
	MF: Fn(I::Item) -> B,
{
	hypothetical: Stream<I, RF, MF, B>,
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
			hypothetical: stream.clone(),
			original: stream,
		}
	}

	pub fn get(&mut self) -> &mut Stream<I, RF, MF, B> {
		self.hypothetical.by_ref()
	}

	pub fn nip(self) {
		*self.original = self.hypothetical;
	}
}
