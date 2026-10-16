use comfy_table::{presets::UTF8_FULL_CONDENSED, Cell, CellAlignment, Color, ContentArrangement, Table};
use owo_colors::{AnsiColors, OwoColorize};
use szu::math::ilog10ceil;

use super::{lexer::{literal::{NumLit, RealLit}, Token}, source::{DocumentMeta, Sourced}};

/// How a token is colored, the same in the source listing and the token table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Class {
	Keyword,
	Literal,
	Str,
	Identifier,
	Operator,
	Punctuation,
	/// Text between tokens, like comments.
	Gap,
}

impl Class {
	fn of(token: &Token) -> Class {
		match token {
			Token::Let | Token::Mut | Token::Discard
			| Token::Return | Token::Break | Token::Continue | Token::Unreachable
			| Token::Plex | Token::Fn
			| Token::Loop | Token::While | Token::For | Token::In | Token::If | Token::Else | Token::Match
			| Token::Ipt | Token::Yield => Class::Keyword,
			Token::True | Token::False | Token::NumLit(_) => Class::Literal,
			Token::StrLit(_) => Class::Str,
			Token::Identifier(_) => Class::Identifier,
			Token::Dot | Token::Comma | Token::Colon | Token::Semicolon
			| Token::LParen | Token::RParen | Token::LBracket | Token::RBracket | Token::LBrace | Token::RBrace => Class::Punctuation,
			Token::LineComment | Token::BlockComment => Class::Gap,
			_ => Class::Operator,
		}
	}

	fn color(self) -> Option<AnsiColors> {
		match self {
			Class::Keyword => Some(AnsiColors::Magenta),
			Class::Literal => Some(AnsiColors::Cyan),
			Class::Str => Some(AnsiColors::Green),
			Class::Identifier => None,
			Class::Operator => Some(AnsiColors::Yellow),
			Class::Punctuation => Some(AnsiColors::White),
			Class::Gap => Some(AnsiColors::BrightBlack),
		}
	}

	fn table_color(self) -> Option<Color> {
		match self {
			Class::Keyword => Some(Color::Magenta),
			Class::Literal => Some(Color::Cyan),
			Class::Str => Some(Color::Green),
			Class::Identifier => None,
			Class::Operator => Some(Color::Yellow),
			Class::Punctuation => Some(Color::White),
			Class::Gap => Some(Color::DarkGrey),
		}
	}
}

const TAB: &str = "    ";

/// Prints the source with line numbers, highlighted by its tokens when lexing worked.
pub fn print_source(document: &DocumentMeta, tokens: Option<&[Sourced<Token>]>) {
	let text = document.get_text();
	// the class of every byte, so a line can be split into colored pieces
	let mut classes = vec![Class::Gap; text.len()];
	match tokens {
		Some(tokens) => for token in tokens {
			let range = token.source();
			classes[range.begin.byte_index()..range.end.byte_index()].fill(Class::of(token));
		},
		None => classes.fill(Class::Identifier), // plain text
	}

	let width = ilog10ceil(document.get_num_lines()).max(1);
	println!("{}", format!("{:>width$} ╭─ {}", "", document.get_name()).bright_black());
	for line in document.lines() {
		let begin = line.range().begin().byte_index();
		let line_str = line.range().get_str().trim_end_matches(['\r', '\n']);

		let mut highlighted = String::new();
		let mut piece_begin = 0;
		for (i, _) in line_str.char_indices().skip(1).chain([(line_str.len(), ' ')]) {
			if i < line_str.len() && classes[begin + i] == classes[begin + piece_begin] {
				continue;
			}
			let piece = line_str[piece_begin..i].replace('\t', TAB);
			match classes[begin + piece_begin].color() {
				Some(color) => highlighted.push_str(&piece.color(color).to_string()),
				None => highlighted.push_str(&piece),
			}
			piece_begin = i;
		}
		println!("{} {}", format!("{:>width$} │", line.to_string()).bright_black(), highlighted);
	}
	println!("{}", format!("{:>width$} ╰─", "").bright_black());
}

/// Prints the tokens as a table, with where they are, their kind, their text and the value the lexer read from them.
pub fn print_tokens(document: &DocumentMeta, tokens: &[Sourced<Token>]) {
	let mut table = Table::new();
	table
		.load_style(UTF8_FULL_CONDENSED.with_rounded_corners())
		.set_content_arrangement(ContentArrangement::Dynamic)
		.set_header(["#", "Location", "Kind", "Text", "Value"]);
	table.column_mut(0).unwrap().set_cell_alignment(CellAlignment::Right);

	for (i, token) in tokens.iter().enumerate() {
		let range = token.source().with_meta(document);
		let class = Class::of(token);
		let kind = Cell::new(crate::compiler::lexer::TokenTag::from(&**token));
		let kind = match class.table_color() {
			Some(color) => kind.fg(color),
			None => kind,
		};
		let text = escape_control(range.get_str());
		let value = match &**token {
			Token::Identifier(name) => name.clone(),
			Token::StrLit(value) => format!("{:?}", value),
			Token::NumLit(NumLit::Int(lit)) => match lit.ty {
				Some(ty) => format!("{} {}", lit.value, ty),
				None => lit.value.to_string(),
			},
			Token::NumLit(NumLit::Real(lit)) => {
				let value = RealLit { num_bits: None, ..lit.clone() }.to_string();
				match lit.num_bits {
					Some(bits) => format!("{} f{}", value, bits),
					None => value,
				}
			},
			Token::True => "true".to_string(),
			Token::False => "false".to_string(),
			_ => String::new(),
		};
		table.add_row([
			Cell::new(i),
			Cell::new(range.to_string()),
			kind,
			Cell::new(text),
			Cell::new(value),
		]);
	}
	println!("{table}");
}

/// Escapes control characters, like the newline in a multi-line string, so a token stays on one row.
fn escape_control(text: &str) -> String {
	text.chars()
		.map(|c| if c.is_control() { c.escape_debug().to_string() } else { c.to_string() })
		.collect()
}
