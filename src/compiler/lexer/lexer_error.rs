use std::{fmt::Display, ops::Deref};

use color_print::cformat;
use unicode_width::UnicodeWidthStr;

use crate::compiler::{lexer::Token, map_peekable::soft_error::SoftResult, source::{self, Sourced, SourcedMeta}};

#[derive(Debug, thiserror::Error)]
pub enum LexerErrorKind {
	#[error("Stream was exhausted")]
	StreamExhausted,

	#[error("Radix expected a leading zero")]
	RadixSymbolExpectedLeadingZero,
	#[error("Invalid radix symbol '{0}'")]
	InvalidRadixSymbol(char),

	#[error("Int literal has leading underscore, this is not allowed")]
	DigitsHaveLeadingUnderscore,
	#[error("Invalid digit \'{}\'({}) for radix {}", digit_symbol, digit_value, radix)]
	InvalidDigitForRadix {
		digit_symbol: char,
		digit_value: u64,
		radix: u64,
	},
	#[error("Int literal needs at least one digit")]
	IntHasNoDigits,
	#[error("Trailing underscore, this is not allowed")]
	IntHasTrailingUnderscore,
	#[error("Int has no match")]
	IntHasNoMatch,

	#[error("The radix '{0}' is larger than 36. The allowed symbols [0-9a-z] can't encode a radix this large")]
	IntRadixTooLarge(u64),
	#[error("Found unexpected radix in digits")]
	IntFoundRadixInDigits,

	#[error("Multi-line comment not closed")]
	MultilineCommentDelimiterNotClosed,
	#[error("String literal not closed")]
	StringDelimiterNotClosed,
	#[error("Invalid escape sequence '{0}'")]
	InvalidEscapeSequence(char),

	#[error("No token matching stream")]
	NoTokenMatchingStream,
}

pub struct LexerError {
	pub kind: LexerErrorKind,
	pub next_chars_window: Vec<char>,
	pub did_next_chars_window_exhaust_stream: bool,
	pub tokens: Vec<Sourced<Token>>,
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
			let token = SourcedMeta::new_with_document(token.as_ref(), self.document);
			writeln!(f, "{} -> {:?}", token.source(), token.deref())?;
		}

		Ok(())
	}
}

pub type LexerResult<T> = SoftResult<T, LexerErrorKind, LexerErrorKind>;
