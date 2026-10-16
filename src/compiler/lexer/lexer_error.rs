use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::compiler::{lexer::Token, source::{DocumentMeta, Sourced}};

#[derive(Debug, Default, Clone, PartialEq, thiserror::Error)]
pub enum LexerErrorKind {
	#[error("Int literal has a leading underscore, this is not allowed")]
	DigitsHaveLeadingUnderscore,
	#[error("Invalid digit '{digit_symbol}'({digit_value}) for radix {radix}")]
	InvalidDigitForRadix {
		digit_symbol: char,
		digit_value: u64,
		radix: u64,
	},
	#[error("Int literal needs at least one digit")]
	IntHasNoDigits,
	#[error("Int literal has a trailing underscore, this is not allowed")]
	IntHasTrailingUnderscore,
	#[error("Int literal does not fit in 64 bits")]
	IntOverflow,
	#[error("The radix '{0}' is larger than 36. The allowed symbols [0-9a-z] can't encode a radix this large")]
	IntRadixTooLarge(u64),

	#[error("Multi-line comment not closed")]
	MultilineCommentDelimiterNotClosed,
	#[error("String literal not closed")]
	StringDelimiterNotClosed,
	#[error("Invalid escape sequence '{0}'")]
	InvalidEscapeSequence(String),

	#[default]
	#[error("No token matching stream")]
	NoTokenMatchingStream,
}

pub struct LexerError {
	pub kind: LexerErrorKind,
	/// Range of the failing text, counted in characters (not bytes). Ariadne uses characters.
	pub span: Range<usize>,
	/// Every token that lexed correctly before the error.
	pub tokens: Vec<Sourced<Token>>,
}

impl LexerError {
	pub fn eprint(&self, document: &DocumentMeta) {
		let report = Report::build(
			ReportKind::Error,
			(document.get_name().to_string(), self.span.clone()))
			.with_message(self.kind.to_string())
			.with_label(
				Label::new((document.get_name().to_string(), self.span.clone()))
					.with_message(self.kind.to_string())
					.with_color(Color::Red),
			);

		let _ = report
			.finish()
			.eprint((document.get_name().to_string(), Source::from(document.get_text())));
	}
}
