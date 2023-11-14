use super::Stream;

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
		//debug_assert!(self.hypothetical.is_none(), "stream hypothetical not collapsed");
	}
}
