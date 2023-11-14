use std::fmt::Display;

struct Column(usize);

impl Column {
	pub fn new(index: usize) -> Self {
		Self(index)
	}
}

impl Display for Column {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, self.0)
    }
}
