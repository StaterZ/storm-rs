use super::super::ResultSH;

#[derive(Debug)]
pub enum ResultSHKind {
	Success,
	SoftErr,
	HardErr,
}

impl<T, E> From<&ResultSH<T, E>> for ResultSHKind {
    fn from(value: &ResultSH<T, E>) -> Self {
        match value {
			Ok(Ok(_)) => Self::Success,
			Ok(Err(_)) => Self::SoftErr,
			Err(_) => Self::HardErr,
		}
    }
}
