use super::soft_error::{SoftError, SoftResult};
use super::PeekableIterator;

pub enum PeekableNextIfError<T> {
	StreamExhausted,
	PredicateError(T),
}

pub trait PeekIterUtils: PeekableIterator {
	fn next_if_map<T>(
		&mut self,
		pred: impl FnOnce(&Self::Item) -> Option<T>
	) -> Option<(Self::Item, T)> {
		self
			.peek()
			.and_then(pred)
			.map(|value| (self.next().unwrap(), value)) //unwrap is safe here since we managed to peek something
	}
	
	fn next_if_err<E>(
		&mut self,
		pred: impl FnOnce(&Self::Item) -> Result<(), E>
	) -> Result<Self::Item, PeekableNextIfError<E>> {
		match self.peek() {
			Some(item) => match pred(item) {
				Ok(_) => Ok(self.next().unwrap()), //unwrap is safe here since we managed to peek something
				Err(err) => Err(PeekableNextIfError::PredicateError(err)),
			},
			None => Err(PeekableNextIfError::StreamExhausted),
		}
	}
	
	fn try_rule<T, E>(
		&mut self,
		rule: impl FnOnce(&mut Self) -> Result<T, E>
	) -> Result<T, E> where Self: Clone {
		let recover_state = self.clone();

		let result = rule(self);
		if result.is_err() {
			*self = recover_state;
		}

		result
	}

	fn try_rule_sh<T, ES, EH>(
		&mut self,
		rule: impl FnOnce(&mut Self) -> SoftResult<T, ES, EH>
	) -> SoftResult<T, ES, EH> where Self: Clone {
		let recover_state = self.clone();

		let result = rule(self);
		if matches!(result, Err(SoftError::Soft(_))) {
			*self = recover_state;
		}

		result
	}

	fn try_rule_sh_arg<T, ES, EH, ARG>(
		&mut self,
		rule: impl FnOnce(&mut Self, ARG) -> SoftResult<T, ES, EH>, arg: ARG
	) -> SoftResult<T, ES, EH> where Self: Clone {
		let recover_state = self.clone();

		let result = rule(self, arg);
		if matches!(result, Err(SoftError::Soft(_))) {
			*self = recover_state;
		}

		result
	}
}

impl<T: PeekableIterator> PeekIterUtils for T { }
