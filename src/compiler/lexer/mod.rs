mod token;
mod lexer_error;

use phf::phf_map;

pub use token::{Token, TokenTag};

use crate::compiler::{
	lexer::lexer_error::{LexerError, LexerErrorKind, LexerResult}, map_peekable::{
		soft_error::{SoftError, SoftResultTrait}, MapPeekable, PeekIterUtils, PeekableIterator
	}, source::{self, Sourced}
};

pub trait CharStream = PeekableIterator<Item = char> + Clone;

pub fn lex(document: &source::Document) -> Result<Vec<Sourced<Token>>, LexerError> {
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
		match next_token(&mut stream) {
			Ok(token) => {
				let is_eof = token == Token::Eof;

				let end = get_current_source_pos(document, &mut stream);
				tokens.push(Sourced::new(token, source::Range { begin, end }));

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

fn next_token(stream: &mut impl CharStream) -> Result<Token, LexerErrorKind> {
	{
		fn is_space(c: &char) -> bool { matches!(*c, ' ' | '\t') }
		if stream.next_if(is_space).is_some() {
			while stream.next_if(is_space).is_some() { }
			return Ok(Token::Space);
		}
	}

	{
		let r = stream.next_if_eq(&'\r').is_some();
		let n = stream.next_if_eq(&'\n').is_some();
		if r || n {
			return Ok(Token::NewLine);
		}
	}

	if stream.next_if_eq(&'.').is_some() {
		return Ok(Token::Dot);
	}
	if stream.next_if_eq(&',').is_some() {
		return Ok(Token::Comma);
	}
	if stream.next_if_eq(&':').is_some() {
		return Ok(Token::Colon);
	}
	if stream.next_if_eq(&';').is_some() {
		return Ok(Token::Semicolon);
	}
	
	if stream.next_if_eq(&'(').is_some() {
		return Ok(Token::LParen);
	}
	if stream.next_if_eq(&')').is_some() {
		return Ok(Token::RParen);
	}
	if stream.next_if_eq(&'[').is_some() {
		return Ok(Token::LBracket);
	}
	if stream.next_if_eq(&']').is_some() {
		return Ok(Token::RBracket);
	}
	if stream.next_if_eq(&'{').is_some() {
		return Ok(Token::LBrace);
	}
	if stream.next_if_eq(&'}').is_some() {
		return Ok(Token::RBrace);
	}

	if stream.next_if_eq(&'+').is_some() {
		return Ok(Token::Plus);
	}
	if stream.next_if_eq(&'-').is_some() {
		return Ok(Token::Dash);
	}
	if stream.next_if_eq(&'*').is_some() {
		return Ok(Token::Star);
	}
	if stream.next_if_eq(&'/').is_some() {
		if stream.next_if_eq(&'/').is_some() {
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(Token::Comment);
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
			return Ok(Token::MultilineComment);
		}
		
		return Ok(Token::Slash);
	}
	if stream.next_if_eq(&'%').is_some() {
		return Ok(Token::Percent);
	}
	if stream.next_if_eq(&'#').is_some() {
		return Ok(Token::Hash);
	}
	if stream.next_if_eq(&'<').is_some() {
		if stream.next_if_eq(&'<').is_some() {
			return Ok(Token::LShift);
		}
		if stream.next_if_eq(&'=').is_some() {
			return Ok(Token::Le);
		}

		return Ok(Token::Lt);
	}
	if stream.next_if_eq(&'>').is_some() {
		if stream.next_if_eq(&'>').is_some() {
			return Ok(Token::RShift);
		}
		if stream.next_if_eq(&'=').is_some() {
			return Ok(Token::Ge);
		}

		return Ok(Token::Gt);
	}

	if stream.next_if_eq(&'=').is_some() {
		if stream.next_if_eq(&'=').is_some() {
			return Ok(Token::Eq);
		}

		return Ok(Token::Equals);
	}

	if stream.next_if_eq(&'!').is_some() {
		if stream.next_if_eq(&'=').is_some() {
			return Ok(Token::Ne);
		}
		return Ok(Token::Bang);
	}

	if stream.next_if_eq(&'&').is_some() {
		if stream.next_if_eq(&'&').is_some() {
			return Ok(Token::And);
		}
		return Ok(Token::Ampersand);
	}

	if stream.next_if_eq(&'|').is_some() {
		if stream.next_if_eq(&'|').is_some() {
			return Ok(Token::Or);
		}
		return Ok(Token::Bar);
	}

	if let Ok(value) = stream.try_rule_sh(parse_int).to_nested()? {
		return Ok(Token::IntLit(value));
	}

	{
		let ident_str = stream.next_if_eq(&'@').is_some();
		if stream.next_if_eq(&'\"').is_some() {
			let mut value = String::new();
			while stream.next_if_eq(&'\"').is_none() {
				if stream.next_if_eq(&'\\').is_some() {
					static ESCAPE_CODES: phf::Map<char, char> = phf_map! {
						// 'b' => '\b',
						// 'e' => '\e',
						// 'f' => '\f',
						'r' => '\r',
						't' => '\t',
						// 'v' => '\v',
						'n' => '\n',
						'"' => '"',
						'\\' => '\\',
					};
					
					let c = stream.next().ok_or(LexerErrorKind::StringDelimiterNotClosed)?;
					let Some(&escaped) = ESCAPE_CODES.get(&c) else {
						return Err(LexerErrorKind::InvalidEscapeSequence(c));
					};
					value.push(escaped);
					continue;
				}

				let c = stream.next().ok_or(LexerErrorKind::StringDelimiterNotClosed)?;
				value.push(c);
			}

			return Ok(if ident_str {
				Token::Identifier(value)
			} else {
				Token::StrLit(value)
			});
		}
	}

	if let Some(ident_begin) = stream.next_if(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		let mut value = ident_begin.to_string();
		while let Some(ident) = stream.next_if(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
			value.push(ident);
		}

		static KEYWORDS: phf::Map<&'static str, TokenTag> = phf_map! {
			"let" => TokenTag::Let,
			"mut" => TokenTag::Mut,
			"_" => TokenTag::Discard,
			
			"ret" => TokenTag::Return,
			"break" => TokenTag::Break,
			"continue" => TokenTag::Continue,
			"unreachable" => TokenTag::Unreachable,
			
			"plex" => TokenTag::Plex,

			"fn" => TokenTag::Fn,
			"loop" => TokenTag::Loop,
			"while" => TokenTag::While,
			"for" => TokenTag::For,
			"in" => TokenTag::In,
			"if" => TokenTag::If,
			"else" => TokenTag::Else,

			"ipt" => TokenTag::Ipt,
			"yield" => TokenTag::Yield,

			"true" => TokenTag::True,
			"false" => TokenTag::False,
		};
		
		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.try_into().unwrap()); //unwrap here is safe since the KEYWORDS list will never list a type with a value
		}
		
		return Ok(Token::Identifier(value));
	}

	if stream.peek().is_none() {
		return Ok(Token::Eof);
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
			None => Err(SoftError::new(LexerErrorKind::InvalidRadixSymbol(*c), matches!(c, 'a'..='z' | 'A'..='Z')))
		},
		None => Err(SoftError::Soft(LexerErrorKind::StreamExhausted)),
	}

}

fn parse_digits(
	stream: &mut impl CharStream,
	radix: u64,
	mut has_consumed: bool,
	allow_radix_end: bool,
) -> LexerResult<u64> {
	let mut has_trailing_underscore = false;
	let mut value = 0u64;
	let mut i = 0usize;
	while let Some(&c) = stream.peek() {
		if c == '_' {
			if i == 0 {
				return Err(SoftError::new(LexerErrorKind::DigitsHaveLeadingUnderscore, has_consumed));
			}

			has_trailing_underscore = true;
		} else {
			match c.to_digit(36) {
				Some(digit_value) => {
					let digit_value = digit_value as u64;
					
					if digit_value >= radix {
						if allow_radix_end && c == 'r' { break; }

						return Err(SoftError::new(LexerErrorKind::InvalidDigitForRadix {
							digit_symbol: c,
							digit_value,
							radix
						}, has_consumed));
					}
					value *= radix;
					value += digit_value;
		
					has_consumed = true;
					has_trailing_underscore = false;
				},
				None => break,
			};
		}

		stream.next();
		i += 1;
	}
	
	
	if !has_consumed {
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
