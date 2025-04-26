use std::fmt::Display;

use owo_colors::{DynColors, OwoColorize};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum SymbolKind {
	Fork,
	Line,
	Turn,
	None,
}

#[derive(Debug)]
struct Symbol {
	kind: SymbolKind,
	color: DynColors,
}

impl Display for SymbolKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			SymbolKind::Fork => write!(f, "├→"),
			SymbolKind::Line => write!(f, "│ "),
			SymbolKind::Turn => write!(f, "╰→"),
			SymbolKind::None => write!(f, "· "),
		}
	}
}

impl Display for Symbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.kind.to_string().color(self.color))
	}
}

pub struct Indent {
	symbols: Vec<Symbol>,
}

impl Indent {
	pub fn new() -> Self {
		Self{
			symbols: vec![],
		}
	}

	pub fn push(&mut self, color: DynColors, is_last: bool) {
		self.symbols.push(Symbol {
			kind: if is_last { SymbolKind::Turn } else { SymbolKind::Fork },
			color,
		});
	}

	pub fn pop(&mut self) {
		self.symbols.pop();
	}

	pub fn extend(&mut self) {
		if let Some(last) = self.symbols.last_mut() {
			last.kind = match last.kind {
				SymbolKind::Fork => SymbolKind::Line,
				SymbolKind::Turn => SymbolKind::None,
				kind => kind,
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
