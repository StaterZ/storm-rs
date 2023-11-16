pub use token::Token;
pub use token_meta::TokenMeta;
pub use token_kind::{TokenKind, TokenKindTag};

use std::collections::HashMap;
use lazy_static::lazy_static;

use crate::compiler::source::SourcePosMeta;

use super::{
	stream::{Stream, StreamExt},
	source::{SourcePos, SourceRange, SourceFile},
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
>) -> Result<Option<u64>, String> {
	let mut stream = stream.dup();

	if stream.get().expect_eq(&'0').is_some() {
		lazy_static!{
			static ref RADICES: HashMap<char, u64> = vec![
				('b', 2),
				('q', 4),
				('o', 8),
				('d', 10),
				('x', 16),
			].into_iter().collect();
		}

		match stream.get().get_peeker().get_current() {
			Some(c) => match RADICES.get(&c) {
				Some(&radix) => {
					stream.get().next();
					stream.nip();
					return Ok(Some(radix));
				},
				None => match c {
					'a'..='z' | 'A'..='Z' => {
						let msg = format!("Invalid radix symbol \'{:?}\'", c);
						stream.nip();
						return Err(msg);
					},
					_ => stream.pop(),
				}
			},
			None => stream.pop(),
		};
	} else {
		stream.pop();
	}

	Ok(None)
}

fn parse_int(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> Result<Option<u64>, String> {
	let mut stream = stream.dup();

	let radix = parse_radix(stream.get())?;
	let mut is_match = radix.is_some();
	let radix = radix.unwrap_or(10);

	let mut has_trailing_underscore = false;
	let mut value = 0u64;
	let mut i = 0usize;
	while let Some(&c) = stream.get().get_peeker().get_current() {
		if c == '_' {
			if i == 0 {
				stream.pop();
				return Err("Leading underscore, this is not allowed".to_string());
			}

			has_trailing_underscore = true;
		} else {
			match c.to_digit(36) {
				Some(digit_value) => {
					let digit_value = digit_value as u64;
					
					if digit_value >= radix {
						let result = if is_match {
							Err(format!("Invalid digit \'{}\' for radix {}", c, radix))
						} else {
							Ok(None)
						};
						stream.pop();

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

		stream.get().next();
		i += 1;
	}

	if !is_match {
		stream.pop();
		return Ok(None);
	}

	if has_trailing_underscore {
		stream.pop();
		return Err("Trailing underscore, this is not allowed".to_string());
	}

	if stream.get().check(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		stream.pop();
		return Ok(None);
	}
	
	stream.nip();
	Ok(Some(value))
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

	if let Some(value) = parse_int(stream)? {
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
				("ipt", TokenKindTag::Ipt),
				("yield", TokenKindTag::Yield),

				("get", TokenKindTag::KeywordGet),
				("set", TokenKindTag::KeywordSet),
				("mov", TokenKindTag::KeywordMov),
				("add", TokenKindTag::KeywordAdd),
				("sub", TokenKindTag::KeywordSub),
				("mul", TokenKindTag::KeywordMul),
				("div", TokenKindTag::KeywordDiv),
				("lbl", TokenKindTag::KeywordLbl),
				("jmp", TokenKindTag::KeywordJmp),
				("beqz", TokenKindTag::KeywordBeqz),
				("bltz", TokenKindTag::KeywordBltz),
			].into_iter().collect();
		}

		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.into());
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

pub fn lex(file: &SourceFile) -> Result<Vec<Token>, String> {
	let mut stream = file.chars().enumerate().stream(
		|(_, c)| c,
		|(_, c)| c,
	);
	
	fn get_current_source_pos<'a>(file: &'a SourceFile, stream: &mut Stream<
		impl ExactSizeIterator<Item = (usize, char)>,
		impl Fn(&(usize, char)) -> &char,
		impl Fn((usize, char)) -> char,
		char,
	>) -> SourcePosMeta<'a> {
		let pos = stream
			.get_peeker()
			.get_current_raw()
			.map(|&(i, _)| SourcePos::new(i))
			.unwrap_or_else(|| SourcePos::new(file.chars().len()));

		SourcePosMeta {
			pos,
			file,
		}
	}
		
	let mut tokens = Vec::new();
	let mut begin = get_current_source_pos(file, &mut stream);
	loop {
		match next_token_kind(&mut stream) {
			Ok(kind) => {
				let is_eof = kind == TokenKind::Eof;

				let end = get_current_source_pos(file, &mut stream);
				tokens.push(Token{
					kind: kind,
					source: SourceRange::new(begin, end),
				});

				if is_eof {
					return Ok(tokens);
				}

				begin = end;
			},
			Err(err) => return Err(format!("[pos:{}] {}", begin.pos.char_index(), err)),
		}
	}
}
