use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Symbol {
	Fork,
	Line,
	Turn,
	None,
}

impl Display for Symbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Symbol::Fork => write!(f, "├→"),
			Symbol::Line => write!(f, "│ "),
			Symbol::Turn => write!(f, "╰→"),
			Symbol::None => write!(f, "· "),
		}
	}
}

pub struct Indent {
	symbols: Vec<Symbol>,
}

impl Indent {
	pub fn new() -> Self {
		Self{
			symbols: vec!(),
		}
	}

	pub fn push(&mut self, is_last: bool) {
		self.symbols.push(if is_last { Symbol::Turn } else { Symbol::Fork });
	}

	pub fn pop(&mut self) {
		self.symbols.pop();
	}

	pub fn extend(&mut self) {
		if let Some(last) = self.symbols.last_mut() {
			*last = match last {
				Symbol::Fork => Symbol::Line,
				Symbol::Turn => Symbol::None,
				_ => *last,
			}
		}
	}
}


impl Display for Indent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for c in self.symbols.iter() {
			write!(f, "{}", c)?;
		}
		Ok(())
	}
}