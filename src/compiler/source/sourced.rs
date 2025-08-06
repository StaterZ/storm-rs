use std::ops::{Deref, DerefMut};

use szu::mappable::Mappable;

use super::Range;

#[derive(Clone, Copy, Debug)]
pub struct Sourced<T> {
	value: T,
	range: Range,
}

impl<T> Sourced<T> {
	pub fn new(value: T, range: Range) -> Self {
		Self {
			value,
			range,
		}
	}

	pub fn cast<U: From<T>>(self) -> Sourced<U> {
		self.map(|x| x.into())
	}

	pub fn inner(self) -> T {
		self.value
	}

	pub fn as_ref(&self) -> Sourced<&T> {
		Sourced {
			value: &self.value,
			range: self.range,
		}
	}

	pub fn as_mut(&mut self) -> Sourced<&mut T> {
		Sourced {
			value: &mut self.value,
			range: self.range,
		}
	}

	pub fn source(&self) -> &Range {
		&self.range
	}
}

impl<T> Sourced<Option<T>> {
	pub fn transpose(self) -> Option<Sourced<T>> {
		self.value.map(|value| Sourced {
			value,
			range: self.range,
		})
	}
}
impl<T, E> Sourced<Result<T, E>> {
	pub fn transpose(self) -> Result<Sourced<T>, E> {
		self.value.map(|value| Sourced {
			value,
			range: self.range,
		})
	}
}

impl<T> Mappable<T> for Sourced<T> {
	type Target<U> = Sourced<U>;

	fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Self::Target<U> {
		Sourced {
			value: f(self.value),
			range: self.range,
		}
	}
}

impl<I: Iterator> Iterator for Sourced<I> {
	type Item = Sourced<I::Item>;

	fn next(&mut self) -> Option<Self::Item> {
		self.value
			.next()
			.map(|value| Sourced::new(value, self.range))
	}
}

impl<T> Deref for Sourced<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.value
	}
}

impl<T> DerefMut for Sourced<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.value
	}
}
