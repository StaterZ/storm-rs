pub use token::Token;
pub use token_meta::TokenMeta;
pub use token_kind::{TokenKind, TokenKindTag};

use std::collections::HashMap;
use lazy_static::lazy_static;

use super::{
	stream::{Stream, StreamExt},
	source,
};

mod token;
mod token_meta;
mod token_kind;

trait CharStreamIter = Iterator<Item = (usize, char)>;
trait CharStreamRF = for<'a> Fn(&'a (usize, char)) -> &'a char;
trait CharStreamMF = Fn((usize, char)) -> char;
type CharStream<
	I/*: CharStreamIter*/,
	RF/*: CharStreamRF*/,
	MF/*: CharStreamMF*/,
> = Stream<I, RF, MF, char>;

fn parse_radix(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> Result<Result<u64, ()>, String> {
	if stream.expect_eq(&'0').is_none() {
		return Ok(Err(()));
	}

	lazy_static!{
		static ref RADICES: HashMap<char, u64> = vec![
			('b', 2),
			('q', 4),
			('o', 8),
			('d', 10),
			('x', 16),
		].into_iter().collect();
	}

	if let Some(c) = stream.get_peeker().get_current() {
		match RADICES.get(&c) {
			Some(&radix) => {
				stream.next();
				return Ok(Ok(radix));
			},
			None => if matches!(c, 'a'..='z' | 'A'..='Z') {
				return Err(format!("Invalid radix symbol \'{}\'", c));
			}
		}
	}

	Ok(Err(()))
}

fn parse_int(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> Result<Result<u64, ()>, String> {
	let radix = stream.try_rule_hs(parse_radix)?;
	let mut digits = parse_digits(stream, radix.unwrap_or(10), radix.is_ok(), true)?;

	if radix.is_err() {
		if let Ok(radix) = digits {
			if stream.expect_eq(&'r').is_some() {
				if radix > 36 {
					return Err(format!("The radix '{}' is larger than 36. The allowed symbols [0-9a-z] can't encode a radix this large", radix));
				}
				digits = parse_digits(stream, radix, true, false)?;
				if digits.is_err() {
					return Err("found radix".to_string());
				}
			}
		}
	}
	
	Ok(digits)
}

fn parse_digits(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>, radix: u64, mut is_match: bool, allow_radix_end: bool) -> Result<Result<u64, ()>, String> {
	let mut has_trailing_underscore = false;
	let mut value = 0u64;
	let mut i = 0usize;
	while let Some(&c) = stream.get_peeker().get_current() {
		if c == '_' {
			if i == 0 {
				return Err("Leading underscore, this is not allowed".to_string());
			}

			has_trailing_underscore = true;
		} else {
			match c.to_digit(36) {
				Some(digit_value) => {
					let digit_value = digit_value as u64;
					
					if digit_value >= radix {
						if allow_radix_end && c == 'r' {
							break;
						}

						let result = if is_match {
							Err(format!("Invalid digit \'{}\'({}) for radix {}", c, digit_value, radix))
						} else {
							Ok(Err(()))
						};

						return result;
					}
		
					value *= radix;
					value += digit_value;
		
					is_match = true;
					has_trailing_underscore = false;
				},
				None => break,
			};
		}

		stream.next();
		i += 1;
	}
	
	
	if !is_match {
		return Ok(Err(()));
	}
	
	if i == 0 {
		return Err("Literal needs at least one digit".to_string());
	}

	if has_trailing_underscore {
		return Err("Trailing underscore, this is not allowed".to_string());
	}

	Ok(Ok(value))
}

fn next_token_kind(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> Result<TokenKind, String> {
	{
		fn is_space(c: &char) -> bool { matches!(*c, ' ' | '\t') }
		if stream.expect(is_space).is_some() {
			while stream.expect(is_space).is_some() { }
			return Ok(TokenKind::Space);
		}
	}

	{
		let r = stream.expect_eq(&'\r').is_some();
		let n = stream.expect_eq(&'\n').is_some();
		if r || n {
			return Ok(TokenKind::NewLine);
		}
	}

	if stream.expect_eq(&'.').is_some() {
		return Ok(TokenKind::Dot);
	}
	if stream.expect_eq(&',').is_some() {
		return Ok(TokenKind::Comma);
	}
	if stream.expect_eq(&';').is_some() {
		return Ok(TokenKind::Semicolon);
	}
	
	if stream.expect_eq(&'(').is_some() {
		return Ok(TokenKind::LParen);
	}
	if stream.expect_eq(&')').is_some() {
		return Ok(TokenKind::RBrace);
	}
	if stream.expect_eq(&'[').is_some() {
		return Ok(TokenKind::LBracket);
	}
	if stream.expect_eq(&']').is_some() {
		return Ok(TokenKind::RBracket);
	}
	if stream.expect_eq(&'{').is_some() {
		return Ok(TokenKind::LBrace);
	}
	if stream.expect_eq(&'}').is_some() {
		return Ok(TokenKind::RBrace);
	}

	if stream.expect_eq(&'+').is_some() {
		return Ok(TokenKind::Plus);
	}
	if stream.expect_eq(&'-').is_some() {
		return Ok(TokenKind::Dash);
	}
	if stream.expect_eq(&'*').is_some() {
		return Ok(TokenKind::Star);
	}
	if stream.expect_eq(&'/').is_some() {
		if stream.expect_eq(&'/').is_some() {
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::Comment);
		}
		if stream.expect_eq(&'*').is_some() {
			let mut indent = 0usize;
			while indent > 0 {
				match stream.next() {
					Some(c) => match c {
						'/' if stream.expect_eq(&'*').is_some() => indent += 1,
						'*' if stream.expect_eq(&'/').is_some() => indent -= 1,
						_ => {},
					},
					None => return Err("Multi-line comment not closed".to_string()),
				}
			}
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::MultilineComment);
		}
		
		return Ok(TokenKind::Slash);
	}
	if stream.expect_eq(&'%').is_some() {
		return Ok(TokenKind::Percent);
	}
	if stream.expect_eq(&'<').is_some() {
		if stream.expect_eq(&'<').is_some() {
			return Ok(TokenKind::LShift);
		}

		return Ok(TokenKind::LessThan);
	}
	if stream.expect_eq(&'>').is_some() {
		if stream.expect_eq(&'>').is_some() {
			return Ok(TokenKind::RShift);
		}

		return Ok(TokenKind::GreaterThan);
	}

	if stream.expect_eq(&'=').is_some() {
		if stream.expect_eq(&'=').is_some() {
			return Ok(TokenKind::Equality);
		}

		return Ok(TokenKind::Equals);
	}

	if let Ok(value) = stream.try_rule_hs(parse_int)? {
		return Ok(TokenKind::IntLit(value));
	}

	{
		let ident_str = stream.expect_eq(&'@').is_some();
		if stream.expect_eq(&'\"').is_some() {
			let mut value = String::new();
			while stream.expect_eq(&'\"').is_none() {
				stream.expect_eq(&'\\');
				match stream.next() {
					Some(c) => value.push(c),
					None => return Err("String literal not closed".to_string()),
				}
			}

			return Ok(if ident_str {
				TokenKind::Identifier(value)
			} else {
				TokenKind::StrLit(value)
			});
		}
	}

	if let Some(ident_begin) = stream.expect(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		let mut value = ident_begin.to_string();
		while let Some(ident) = stream.expect(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
			value.push(ident);
		}

		lazy_static!{
			static ref KEYWORDS: HashMap<&'static str, TokenKindTag> = vec![
				("let", TokenKindTag::Let),
				("if", TokenKindTag::If),
				("else", TokenKindTag::Else),

				("ipt", TokenKindTag::Ipt),
				("yield", TokenKindTag::Yield),
			].into_iter().collect();
		}

		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.try_into().unwrap()); //unwrap here is safe since the KEYWORDS list will never list a type with a value
		}
		
		return Ok(TokenKind::Identifier(value));
	}

	if stream.get_peeker().get_current().is_none() {
		return Ok(TokenKind::Eof);
	}

	return Err(format!("No token factory matching stream '{}'", match stream.get_peeker().get_current() {
		Some(c) => c.to_string(),
		None => "EOF".to_string(),
	}));
}

pub fn lex(document: &source::Document) -> Result<Vec<Token>, String> {
	let mut stream = document.chars().enumerate().stream(
		|(_, c)| c,
		|(_, c)| c,
	);
	
	fn get_current_source_pos<'a>(document: &'a source::Document, stream: &mut Stream<
		impl ExactSizeIterator<Item = (usize, char)>,
		impl Fn(&(usize, char)) -> &char,
		impl Fn((usize, char)) -> char,
		char,
	>) -> source::PosMeta<'a> {
		stream
			.get_peeker()
			.get_current_raw()
			.map(|&(i, _)| source::Pos::new(i))
			.unwrap_or_else(|| source::Pos::new(document.chars().len()))
			.to_meta(document)
	}
		
	let mut tokens = Vec::new();
	let mut begin = get_current_source_pos(document, &mut stream);
	loop {
		match next_token_kind(&mut stream) {
			Ok(kind) => {
				let is_eof = kind == TokenKind::Eof;

				let end = get_current_source_pos(document, &mut stream);
				tokens.push(Token{
					kind: kind,
					range: source::Range::new(begin, end),
				});

				if is_eof {
					return Ok(tokens);
				}

				begin = end;
			},
			Err(err) => return Err(format!("{}\n{}", err, source::error_gen::generate_error_line(get_current_source_pos(document, &mut stream).to_range()))),
		}
	}
}
