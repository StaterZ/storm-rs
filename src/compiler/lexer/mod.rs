mod token;
mod token_meta;
mod token_kind;

use std::fmt::Display;

use phf::phf_map;
use color_print::cformat;
use unicode_width::UnicodeWidthStr;

pub use token::Token;
pub use token_meta::TokenMeta;
pub use token_kind::{TokenKind, TokenKindTag};

use super::{
	stream::{Stream, StreamExt},
	source,
	ResultSH,
};


trait CharStreamIter = Iterator<Item = (usize, char)>;
trait CharStreamRF = for<'a> Fn(&'a (usize, char)) -> &'a char;
trait CharStreamMF = Fn((usize, char)) -> char;
type CharStream<
	I/*: CharStreamIter*/,
	RF/*: CharStreamRF*/,
	MF/*: CharStreamMF*/,
> = Stream<I, RF, MF, char>;

pub enum LexerErrorKind {
	StreamExhausted,

	RadixSymbolExpectedLeadingZero,
	InvalidRadixSymbol(char),

	DigitsHaveLeadingUnderscore,
	InvalidDigitForRadix {
		digit_symbol: char,
		digit_value: u64,
		radix: u64,
	},
	IntHasNoDigits,
	IntHasTrailingUnderscore,
	IntHasNoMatch,

	IntRadixTooLarge(u64),
	IntFoundRadixInDigits,

	MultilineCommentDelimiterNotClosed,
	StringDelimiterNotClosed,

	NoTokenMatchingStream,
}

impl Display for LexerErrorKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LexerErrorKind::StreamExhausted => write!(f, "Stream was exhausted"),
			
			LexerErrorKind::RadixSymbolExpectedLeadingZero => write!(f, "Radix expected a leading zero"),
			LexerErrorKind::InvalidRadixSymbol(c) => write!(f, "Invalid radix symbol '{}'", c),
			
			LexerErrorKind::DigitsHaveLeadingUnderscore => write!(f, "Int literal has leading underscore, this is not allowed"),
			LexerErrorKind::InvalidDigitForRadix { digit_symbol, digit_value, radix } => write!(f, "Invalid digit \'{}\'({}) for radix {}", digit_symbol, digit_value, radix),
			LexerErrorKind::IntHasNoDigits => write!(f, "Int literal needs at least one digit"),
			LexerErrorKind::IntHasTrailingUnderscore => write!(f, "Trailing underscore, this is not allowed"),
			LexerErrorKind::IntHasNoMatch => write!(f, "Int has no match"),
			
			LexerErrorKind::IntRadixTooLarge(radix) => write!(f, "The radix '{}' is larger than 36. The allowed symbols [0-9a-z] can't encode a radix this large", radix),
			LexerErrorKind::IntFoundRadixInDigits => write!(f, "Found unexpected radix in digits"),
			
			LexerErrorKind::MultilineCommentDelimiterNotClosed => write!(f, "Multi-line comment not closed"),
			LexerErrorKind::StringDelimiterNotClosed => write!(f, "String literal not closed"),
			
			LexerErrorKind::NoTokenMatchingStream => write!(f, "No token matching stream"),
		}
	}
}

pub struct LexerError {
	kind: LexerErrorKind,
	next_chars_window: Vec<char>,
	did_next_chars_window_exhaust_stream: bool,
	tokens: Vec<Token>,
}

impl LexerError {
	pub fn with_meta<'a, 'b>(&'a self, document: &'b source::Document) -> LexerErrorMeta<'a, 'b> {
		LexerErrorMeta {
			error: self,
			document,
		}
	}
}

pub struct LexerErrorMeta<'a, 'b> {
	error: &'a LexerError,
	document: &'b source::Document,
}

impl<'a, 'b> Display for LexerErrorMeta<'a, 'b> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}\n", self.error.kind)?;

		let align_inset = |nums: &mut String, chars: &String| {
			let inset = chars.width();
			*nums = format!("{:<inset$}", nums);
		};

		let mut chars = String::new();
		let mut nums = String::new();
		for (i, c) in self.error.next_chars_window.iter().enumerate() {
			align_inset(&mut nums, &chars);
			chars.push_str(format!("{:?} ", c).as_str());
			nums.push_str(format!(" {}", i).as_str());
		}
		if self.error.did_next_chars_window_exhaust_stream {
			chars.push_str(cformat!(" <red>EOF</>").as_str());
		}
		write!(f, "{}", cformat!(
		"<yellow>next chars:</> {}\n\
		 <yellow>           </> <cyan>{}</>\n\
		 <yellow>tokens so far:</>\n", chars, nums))?;
		for token in self.error.tokens.iter() {
			writeln!(f, "{}", token.with_meta(self.document))?;
		}

		Ok(())
	}
}

type LexerResult<T> = ResultSH<T, LexerErrorKind>;


pub fn lex(document: &source::Document) -> Result<Vec<Token>, LexerError> {
	let mut stream = document
		.get_content()
		.chars()
		.enumerate()
		.stream(
			|(_, c)| c,
			|(_, c)| c,
		);
	
	fn get_current_source_pos<'a>(document: &'a source::Document, stream: &mut Stream<
		impl Iterator<Item = (usize, char)>,
		impl Fn(&(usize, char)) -> &char,
		impl Fn((usize, char)) -> char,
		char,
	>) -> source::PosMeta<'a> {
		stream
			.get_peeker()
			.get_raw()
			.map(|&(i, _)| source::Pos::new(i))
			.map_or_else(
				|| document.get_eof(),
				|p| p.to_meta(document))
	}
		
	let mut tokens = vec![];
	let mut begin = get_current_source_pos(document, &mut stream);
	loop {
		match next_token_kind(&mut stream) {
			Ok(kind) => {
				let is_eof = kind == TokenKind::Eof;

				let end = get_current_source_pos(document, &mut stream);
				tokens.push(Token{
					kind,
					range: source::Range::new(begin, end),
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
					did_next_chars_window_exhaust_stream: stream.get_peeker().get().is_none(),
					tokens,
				})
			},
		}
	}
}

fn next_token_kind(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> Result<TokenKind, LexerErrorKind> {
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
	if stream.expect_eq(&':').is_some() {
		return Ok(TokenKind::Colon);
	}
	if stream.expect_eq(&';').is_some() {
		return Ok(TokenKind::Semicolon);
	}
	
	if stream.expect_eq(&'(').is_some() {
		return Ok(TokenKind::LParen);
	}
	if stream.expect_eq(&')').is_some() {
		return Ok(TokenKind::RParen);
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
			let mut indent = 1usize;
			while indent > 0 {
				match stream.next() {
					Some(c) => match c {
						'/' if stream.expect_eq(&'*').is_some() => indent += 1,
						'*' if stream.expect_eq(&'/').is_some() => indent -= 1,
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
	if stream.expect_eq(&'%').is_some() {
		return Ok(TokenKind::Percent);
	}
	if stream.expect_eq(&'<').is_some() {
		if stream.expect_eq(&'<').is_some() {
			return Ok(TokenKind::LShift);
		}
		if stream.expect_eq(&'=').is_some() {
			return Ok(TokenKind::Le);
		}

		return Ok(TokenKind::Lt);
	}
	if stream.expect_eq(&'>').is_some() {
		if stream.expect_eq(&'>').is_some() {
			return Ok(TokenKind::RShift);
		}
		if stream.expect_eq(&'=').is_some() {
			return Ok(TokenKind::Ge);
		}

		return Ok(TokenKind::Gt);
	}

	if stream.expect_eq(&'=').is_some() {
		if stream.expect_eq(&'=').is_some() {
			return Ok(TokenKind::Eq);
		}

		return Ok(TokenKind::Equals);
	}

	if stream.expect_eq(&'!').is_some() {
		if stream.expect_eq(&'=').is_some() {
			return Ok(TokenKind::Ne);
		}

		return Ok(TokenKind::Bang);
	}

	if let Ok(value) = stream.try_rule_sh(parse_int)? {
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

	if let Some(ident_begin) = stream.expect(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		let mut value = ident_begin.to_string();
		while let Some(ident) = stream.expect(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
			value.push(ident);
		}

		static KEYWORDS: phf::Map<&'static str, TokenKindTag> = phf_map! {
			"let" => TokenKindTag::Let,
			
			"ret" => TokenKindTag::Return,
			"break" => TokenKindTag::Break,
			"continue" => TokenKindTag::Continue,
			"unreachable" => TokenKindTag::Unreachable,
			
			"loop" => TokenKindTag::Loop,
			"while" => TokenKindTag::While,
			"for" => TokenKindTag::For,
			"in" => TokenKindTag::In,
			"if" => TokenKindTag::If,
			"else" => TokenKindTag::Else,

			"ipt" => TokenKindTag::Ipt,
			"yield" => TokenKindTag::Yield,
		};
		
		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.try_into().unwrap()); //unwrap here is safe since the KEYWORDS list will never list a type with a value
		}
		
		return Ok(TokenKind::Identifier(value));
	}

	if stream.get_peeker().get().is_none() {
		return Ok(TokenKind::Eof);
	}

	return Err(LexerErrorKind::NoTokenMatchingStream);
}

fn parse_int(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> LexerResult<u64> {
	let radix = stream.try_rule_sh(parse_radix_symbol)?;
	let has_radix_symbol = radix.is_ok();
	let mut digits = parse_digits(
		stream,
		radix.unwrap_or(10),
		has_radix_symbol,
		true
	)?;

	if !has_radix_symbol {
		if let Ok(radix) = digits {
			if stream.expect_eq(&'r').is_some() {
				if radix > 36 {
					return Err(LexerErrorKind::IntRadixTooLarge(radix));
				}

				digits = parse_digits(stream, radix, true, false)?;
				if digits.is_err() {
					return Err(LexerErrorKind::IntFoundRadixInDigits);
				}
			}
		}
	}
	
	Ok(digits)
}

fn parse_radix_symbol(stream: &mut CharStream<
	impl CharStreamIter + Clone,
	impl CharStreamRF + Clone,
	impl CharStreamMF + Clone,
>) -> LexerResult<u64> {
	if stream.expect_eq(&'0').is_none() {
		return Ok(Err(LexerErrorKind::RadixSymbolExpectedLeadingZero));
	}

	static RADICES: phf::Map<char, u64> = phf_map! {
		'b' => 2,
		'q' => 4,
		'o' => 8,
		'd' => 10,
		'x' => 16,
	};

	match stream.get_peeker().get() {
		Some(c) => match RADICES.get(&c) {
			Some(&radix) => {
				stream.next();
				Ok(Ok(radix))
			},
			None => if matches!(c, 'a'..='z' | 'A'..='Z') {
				Err(LexerErrorKind::InvalidRadixSymbol(*c))
			} else {
				Ok(Err(LexerErrorKind::InvalidRadixSymbol(*c)))
			}
		},
		None => Ok(Err(LexerErrorKind::StreamExhausted)),
	}

}

fn parse_digits(
	stream: &mut CharStream<
		impl CharStreamIter + Clone,
		impl CharStreamRF + Clone,
		impl CharStreamMF + Clone,
	>,
	radix: u64,
	mut is_match: bool,
	allow_radix_end: bool,
) -> LexerResult<u64> {
	let mut has_trailing_underscore = false;
	let mut value = 0u64;
	let mut i = 0usize;
	while let Some(&c) = stream.get_peeker().get() {
		if c == '_' {
			if i == 0 {
				return Err(LexerErrorKind::DigitsHaveLeadingUnderscore);
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
							Err(error)
						} else {
							Ok(Err(error))
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
		return Ok(Err(LexerErrorKind::IntHasNoMatch));
	}
	
	if i == 0 {
		return Err(LexerErrorKind::IntHasNoDigits);
	}

	if has_trailing_underscore {
		return Err(LexerErrorKind::IntHasTrailingUnderscore);
	}

	Ok(Ok(value))
}
