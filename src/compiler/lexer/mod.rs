mod token;
mod token_meta;
mod token_kind;
mod lexer_error;

use phf::phf_map;

pub use token::Token;
pub use token_meta::TokenMeta;
pub use token_kind::{TokenKind, TokenKindTag};

use crate::compiler::{
	lexer::lexer_error::{LexerError, LexerErrorKind, LexerResult}, map_peekable::{
		soft_error::{SoftError, SoftResultTrait}, MapPeekable, PeekIterUtils, PeekableIterator
	}, source
};

pub trait CharStream = PeekableIterator<Item = char> + Clone;

pub fn lex(document: &source::Document) -> Result<Vec<Token>, LexerError> {
	let mut stream = document
		.char_positions()
		.peekable()
		.map_peekable(
			|(_, c)| c,
			|(_, c)| c,
		);
	
	fn get_current_source_pos<'a>(document: &'a source::Document, stream: &mut MapPeekable<
		impl PeekableIterator<Item = (source::Pos, char)> + Clone,
		impl for<'b> Fn(&'b (source::Pos, char)) -> &'b char + Clone,
		impl Fn((source::Pos, char)) -> char + Clone,
		char,
	>) -> source::Pos {
		stream
			.as_inner()
			.peek()
			.map_or(document.eof(), |(p, _c)| *p)
	}
	
	let mut tokens = Vec::new();
	let mut begin = get_current_source_pos(document, &mut stream);
	loop {
		match next_token_kind(&mut stream) {
			Ok(kind) => {
				let is_eof = kind == TokenKind::Eof;

				let end = get_current_source_pos(document, &mut stream);
				tokens.push(Token{
					kind,
					range: source::Range { begin, end },
				});

				if is_eof {
					return Ok(tokens);
				}

				begin = end;
			},
			Err(err) => {
				let char_window_size = 10;

				return Err(LexerError {
					kind: err,
					next_chars_window: stream.by_ref().take(char_window_size).collect(),
					did_next_chars_window_exhaust_stream: stream.peek().is_none(),
					tokens,
				})
			},
		}
	}
}

fn next_token_kind(stream: &mut impl CharStream) -> Result<TokenKind, LexerErrorKind> {
	{
		fn is_space(c: &char) -> bool { matches!(*c, ' ' | '\t') }
		if stream.next_if(is_space).is_some() {
			while stream.next_if(is_space).is_some() { }
			return Ok(TokenKind::Space);
		}
	}

	{
		let r = stream.next_if_eq(&'\r').is_some();
		let n = stream.next_if_eq(&'\n').is_some();
		if r || n {
			return Ok(TokenKind::NewLine);
		}
	}

	if stream.next_if_eq(&'.').is_some() {
		return Ok(TokenKind::Dot);
	}
	if stream.next_if_eq(&',').is_some() {
		return Ok(TokenKind::Comma);
	}
	if stream.next_if_eq(&':').is_some() {
		return Ok(TokenKind::Colon);
	}
	if stream.next_if_eq(&';').is_some() {
		return Ok(TokenKind::Semicolon);
	}
	
	if stream.next_if_eq(&'(').is_some() {
		return Ok(TokenKind::LParen);
	}
	if stream.next_if_eq(&')').is_some() {
		return Ok(TokenKind::RParen);
	}
	if stream.next_if_eq(&'[').is_some() {
		return Ok(TokenKind::LBracket);
	}
	if stream.next_if_eq(&']').is_some() {
		return Ok(TokenKind::RBracket);
	}
	if stream.next_if_eq(&'{').is_some() {
		return Ok(TokenKind::LBrace);
	}
	if stream.next_if_eq(&'}').is_some() {
		return Ok(TokenKind::RBrace);
	}

	if stream.next_if_eq(&'+').is_some() {
		return Ok(TokenKind::Plus);
	}
	if stream.next_if_eq(&'-').is_some() {
		return Ok(TokenKind::Dash);
	}
	if stream.next_if_eq(&'*').is_some() {
		return Ok(TokenKind::Star);
	}
	if stream.next_if_eq(&'/').is_some() {
		if stream.next_if_eq(&'/').is_some() {
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::Comment);
		}
		if stream.next_if_eq(&'*').is_some() {
			let mut indent = 1usize;
			while indent > 0 {
				match stream.next() {
					Some(c) => match c {
						'/' if stream.next_if_eq(&'*').is_some() => indent += 1,
						'*' if stream.next_if_eq(&'/').is_some() => indent -= 1,
						_ => {},
					},
					None => return Err(LexerErrorKind::MultilineCommentDelimiterNotClosed),
				}
			}
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::MultilineComment);
		}
		
		return Ok(TokenKind::Slash);
	}
	if stream.next_if_eq(&'%').is_some() {
		return Ok(TokenKind::Percent);
	}
	if stream.next_if_eq(&'#').is_some() {
		return Ok(TokenKind::Hash);
	}
	if stream.next_if_eq(&'<').is_some() {
		if stream.next_if_eq(&'<').is_some() {
			return Ok(TokenKind::LShift);
		}
		if stream.next_if_eq(&'=').is_some() {
			return Ok(TokenKind::Le);
		}

		return Ok(TokenKind::Lt);
	}
	if stream.next_if_eq(&'>').is_some() {
		if stream.next_if_eq(&'>').is_some() {
			return Ok(TokenKind::RShift);
		}
		if stream.next_if_eq(&'=').is_some() {
			return Ok(TokenKind::Ge);
		}

		return Ok(TokenKind::Gt);
	}

	if stream.next_if_eq(&'=').is_some() {
		if stream.next_if_eq(&'=').is_some() {
			return Ok(TokenKind::Eq);
		}

		return Ok(TokenKind::Equals);
	}

	if stream.next_if_eq(&'!').is_some() {
		if stream.next_if_eq(&'=').is_some() {
			return Ok(TokenKind::Ne);
		}
		return Ok(TokenKind::Bang);
	}

	if stream.next_if_eq(&'&').is_some() {
		if stream.next_if_eq(&'&').is_some() {
			return Ok(TokenKind::And);
		}
		return Ok(TokenKind::Ampersand);
	}

	if stream.next_if_eq(&'|').is_some() {
		if stream.next_if_eq(&'|').is_some() {
			return Ok(TokenKind::Or);
		}
		return Ok(TokenKind::Bar);
	}

	if let Ok(value) = stream.try_rule_sh(parse_int).to_nested()? {
		return Ok(TokenKind::IntLit(value));
	}

	{
		let ident_str = stream.next_if_eq(&'@').is_some();
		if stream.next_if_eq(&'\"').is_some() {
			let mut value = String::new();
			while stream.next_if_eq(&'\"').is_none() {
				stream.next_if_eq(&'\\');
				match stream.next() {
					Some(c) => value.push(c),
					None => return Err(LexerErrorKind::StringDelimiterNotClosed),
				}
			}

			return Ok(if ident_str {
				TokenKind::Identifier(value)
			} else {
				TokenKind::StrLit(value)
			});
		}
	}

	if let Some(ident_begin) = stream.next_if(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		let mut value = ident_begin.to_string();
		while let Some(ident) = stream.next_if(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
			value.push(ident);
		}

		static KEYWORDS: phf::Map<&'static str, TokenKindTag> = phf_map! {
			"let" => TokenKindTag::Let,
			"mut" => TokenKindTag::Mut,
			
			"ret" => TokenKindTag::Return,
			"break" => TokenKindTag::Break,
			"continue" => TokenKindTag::Continue,
			"unreachable" => TokenKindTag::Unreachable,
			
			"plex" => TokenKindTag::Plex,

			"loop" => TokenKindTag::Loop,
			"while" => TokenKindTag::While,
			"for" => TokenKindTag::For,
			"in" => TokenKindTag::In,
			"if" => TokenKindTag::If,
			"else" => TokenKindTag::Else,

			"ipt" => TokenKindTag::Ipt,
			"yield" => TokenKindTag::Yield,

			"true" => TokenKindTag::True,
			"false" => TokenKindTag::False,
		};
		
		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.try_into().unwrap()); //unwrap here is safe since the KEYWORDS list will never list a type with a value
		}
		
		return Ok(TokenKind::Identifier(value));
	}

	if stream.peek().is_none() {
		return Ok(TokenKind::Eof);
	}

	return Err(LexerErrorKind::NoTokenMatchingStream);
}

fn parse_int(stream: &mut impl CharStream) -> LexerResult<u64> {
	let radix = stream.try_rule_sh(parse_radix_symbol).shed_hard()?;
	let has_radix_symbol = radix.is_ok();
	let mut digits = parse_digits(
		stream,
		radix.unwrap_or(10),
		has_radix_symbol,
		true
	).shed_hard()?;

	if !has_radix_symbol {
		if let Ok(radix) = digits {
			if stream.next_if_eq(&'r').is_some() {
				if radix > 36 {
					return Err(SoftError::Hard(LexerErrorKind::IntRadixTooLarge(radix)));
				}

				digits = parse_digits(stream, radix, true, false).shed_hard()?;
				if digits.is_err() {
					return Err(SoftError::Hard(LexerErrorKind::IntFoundRadixInDigits));
				}
			}
		}
	}
	
	digits.map_err(|err| SoftError::Soft(err))
}

fn parse_radix_symbol(stream: &mut impl CharStream) -> LexerResult<u64> {
	if stream.next_if_eq(&'0').is_none() {
		return Err(SoftError::Soft(LexerErrorKind::RadixSymbolExpectedLeadingZero));
	}

	static RADICES: phf::Map<char, u64> = phf_map! {
		'b' => 2,
		'q' => 4,
		'o' => 8,
		'd' => 10,
		'x' => 16,
	};

	match stream.peek() {
		Some(c) => match RADICES.get(&c) {
			Some(&radix) => {
				stream.next();
				Ok(radix)
			},
			None => if matches!(c, 'a'..='z' | 'A'..='Z') {
				Err(SoftError::Hard(LexerErrorKind::InvalidRadixSymbol(*c)))
			} else {
				Err(SoftError::Soft(LexerErrorKind::InvalidRadixSymbol(*c)))
			}
		},
		None => Err(SoftError::Soft(LexerErrorKind::StreamExhausted)),
	}

}

fn parse_digits(
	stream: &mut impl CharStream,
	radix: u64,
	mut is_match: bool,
	allow_radix_end: bool,
) -> LexerResult<u64> {
	let mut has_trailing_underscore = false;
	let mut value = 0u64;
	let mut i = 0usize;
	while let Some(&c) = stream.peek() {
		if c == '_' {
			if i == 0 {
				return Err(SoftError::Hard(LexerErrorKind::DigitsHaveLeadingUnderscore));
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

						let error = LexerErrorKind::InvalidDigitForRadix {
							digit_symbol: c,
							digit_value,
							radix
						};

						return if is_match {
							Err(SoftError::Hard(error))
						} else {
							Err(SoftError::Soft(error))
						};
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
		return Err(SoftError::Soft(LexerErrorKind::IntHasNoMatch));
	}
	
	if i == 0 {
		return Err(SoftError::Hard(LexerErrorKind::IntHasNoDigits));
	}

	if has_trailing_underscore {
		return Err(SoftError::Hard(LexerErrorKind::IntHasTrailingUnderscore));
	}

	Ok(value)
}
