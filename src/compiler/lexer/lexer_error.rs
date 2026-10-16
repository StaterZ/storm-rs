use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use num_bigint::BigUint;

use crate::compiler::{lexer::Token, source::{DocumentMeta, Sourced}};

use super::literal::IntType;

#[derive(Debug, Default, Clone, PartialEq, thiserror::Error)]
pub enum LexerErrorKind {
	#[error("Number literal has a leading underscore, this is not allowed")]
	DigitsHaveLeadingUnderscore,
	#[error("Invalid digit '{digit_symbol}'({digit_value}) for radix {radix}")]
	InvalidDigitForRadix {
		digit_symbol: char,
		digit_value: u32,
		radix: u32,
	},
	#[error("Number literal needs at least one digit")]
	NumHasNoDigits,
	#[error("Number literal has a trailing underscore, this is not allowed")]
	NumHasTrailingUnderscore,
	#[error("The radix '{0}' is larger than 36. The allowed symbols [0-9a-z] can't encode a radix this large")]
	RadixTooLarge(BigUint),
	#[error("The radix '{0}' is smaller than 2")]
	RadixTooSmall(BigUint),
	#[error("Exponent needs at least one digit")]
	ExponentHasNoDigits,
	#[error("Exponent is too large")]
	ExponentTooLarge,
	#[error("Invalid literal suffix '{0}', expected i<bits>, u<bits>, f<bits> or b<bits>")]
	InvalidSuffix(String),
	#[error("Real literal can't have the int suffix '{0}'")]
	RealWithIntSuffix(IntType),
	#[error("Literal {value} does not fit in {ty}")]
	NumDoesNotFit {
		value: BigUint,
		ty: IntType,
	},

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
