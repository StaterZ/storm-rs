#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Symbol {
	Fork,
	Line,
	Turn,
	None,
}

impl Symbol {
	pub fn get_symbol(&self) -> &'static str {
		match self {
			Symbol::Fork => "├→",
			Symbol::Line => "│ ",
			Symbol::Turn => "╰→",
			Symbol::None => "· ",
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

	pub fn to_string(&self) -> String {
		self.symbols.iter().map(|s| s.get_symbol()).collect()
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
