use super::Stream;

pub trait StreamExt : Iterator {
	fn stream<RF, MF, B>(self, rf: RF, mf: MF) -> Stream<Self, RF, MF, B> where
		Self: Sized,
		RF: Fn(&Self::Item) -> &B,
		MF: Fn(Self::Item) -> B,
	{
		Stream::new(self, rf, mf)
	}
}

impl<I: Iterator> StreamExt for I {}
