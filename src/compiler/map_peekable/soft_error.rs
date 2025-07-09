
#[derive(Debug)]
pub enum SoftError<S, H> {
	Soft(S),
	Hard(H),
}

impl<E> SoftError<E, E> {
	pub fn value(self) -> E {
		match self {
			SoftError::Hard(err) => err,
			SoftError::Soft(err) => err,
		}
	}
}

pub type SoftResult<T, ES, EH> = Result<T, SoftError<ES, EH>>;
pub type SoftResultUnit = SoftResult<(), (), ()>;

pub trait SoftResultTrait<T, ES, EH> {
	fn shed_hard_raw(self) -> Result<Result<T, ES>, EH>;
	fn shed_hard(self) -> SoftResult<Result<T, ES>, ES, EH>;
}
pub trait SoftResultTraitSame<T, E> : SoftResultTrait<T, E, E> {
	fn force_hard(self) -> Self;
}

impl<T, ES, EH> SoftResultTrait<T, ES, EH> for SoftResult<T, ES, EH> {
	fn shed_hard_raw(self) -> Result<Result<T, ES>, EH> {
		match self {
			Ok(value) => Ok(Ok(value)),
			Err(SoftError::Soft(err)) => Ok(Err(err)),
			Err(SoftError::Hard(err)) => Err(err),
		}
	}
	fn shed_hard(self) -> SoftResult<Result<T, ES>, ES, EH> {
		match self {
			Ok(value) => Ok(Ok(value)),
			Err(SoftError::Soft(err)) => Ok(Err(err)),
			Err(SoftError::Hard(err)) => Err(SoftError::Hard(err)),
		}
	}
}

impl<T, E> SoftResultTraitSame<T, E> for SoftResult<T, E, E> {
	fn force_hard(self) -> Self {
		self.map_err(|err| SoftError::Hard(err.value()))
	}
}
