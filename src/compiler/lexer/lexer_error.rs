use std::fmt::Display;

use color_print::cformat;
use unicode_width::UnicodeWidthStr;

use crate::compiler::{lexer::Token, source, map_peekable::soft_error::SoftResult};

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
	pub kind: LexerErrorKind,
	pub next_chars_window: Vec<char>,
	pub did_next_chars_window_exhaust_stream: bool,
	pub tokens: Vec<Token>,
}

impl LexerError {
	pub fn with_meta<'a, 'b>(&'a self, document: &'b source::DocumentMeta) -> LexerErrorMeta<'a, 'b> {
		LexerErrorMeta {
			error: self,
			document,
		}
	}
}

pub struct LexerErrorMeta<'a, 'b> {
	error: &'a LexerError,
	document: &'b source::DocumentMeta<'b>,
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

pub type LexerResult<T> = SoftResult<T, LexerErrorKind, LexerErrorKind>;
