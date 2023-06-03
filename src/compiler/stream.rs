pub struct Stream<I: Iterator> {
	iter: I,
	pub current: Option<I::Item>,
	pub index: usize,
}

impl<I: Iterator> Stream<I> {
	pub fn new(mut iter: I) -> Self {
		let start = iter.next();
		return Self{
			iter,
			current: start,
			index: 0,
		};
	}

	pub fn get_inner<'a>(&'a self) -> &'a I {
		&self.iter
	}

	#[inline(always)]
	pub fn check(&mut self, pred: impl FnOnce(&I::Item) -> bool) -> bool {
		match &self.current {
			Some(c) => pred(c),
			None => false,
		}
	}
	
	#[inline(always)]
	pub fn expect(&mut self, pred: impl FnOnce(&I::Item) -> bool) -> Option<I::Item> {
		if let Some(c) = &self.current {
			if pred(c) {
				let result = self.next();
				assert!(result.is_some());
				return result;
			}
		}

		return None;
	}

	#[inline(always)]
	pub fn expect_err(&mut self, pred: impl FnOnce(&I::Item) -> Result<(), String>) -> Result<I::Item, String> {
		return match &self.current {
			Some(c) => match pred(c) {
				Ok(()) => Ok(self.next().unwrap()),
				Err(err) => Err(err),
			},
			None => Err("Stream iterator is exhausted".to_string()),
		}
	}
}

impl<I: Iterator + Clone> Stream<I>
	where I::Item: Clone
{
	#[inline(always)]
	pub fn hypothetically<T, E>(&mut self, func: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, E> {
		let mut hypothetical = self.dup();

		let result = func(hypothetical.get());
		hypothetical.nip_or_pop(result.is_ok());

		return result;
	}
	
	pub fn dup<'a>(&mut self) -> StreamHypothetical<I> {
		StreamHypothetical::new(self)
	}
}

impl<I: Iterator + Clone> Clone for Stream<I>
	where I::Item: Clone
{
	fn clone(&self) -> Self {
		Self {
			iter: self.iter.clone(),
			current: self.current.clone(),
			index: self.index.clone()
		}
	}
}

impl<I: Iterator> Iterator for Stream<I> {
	type Item = I::Item;

	fn next(&mut self) -> Option<Self::Item> {
		let prev_current = std::mem::replace(&mut self.current, self.iter.next());
		self.index += 1;
		return prev_current;
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
