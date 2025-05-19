use std::ops::Range;
use logos::{Logos, SpannedIter};
pub use token::Token;

mod token;

pub type Spanned<Tok, Loc, Err> = Result<(Loc, Tok, Loc), Err>;

#[derive(Debug, PartialEq, Clone)]
pub struct LexicalError {
	kind: LexicalErrorKind,
	span: Range<usize>,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexicalErrorKind {
	#[default] Unknown,
	BadInteger(std::num::ParseIntError),
	BadString,
}

pub struct Lexer<'input> {
	token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
	pub fn new(input: &'input str) -> Self {
		Self {
			token_stream: Token::lexer(input).spanned(),
		}
	}
}

impl<'input> Iterator for Lexer<'input> {
	type Item = Spanned<Token, usize, LexicalError>;

	fn next(&mut self) -> Option<Self::Item> {
		self.token_stream
			.next()
			.map(|(token, span)| match token {
				Ok(token) => Ok((span.start, token, span.end)),
				Err(err) => Err(LexicalError { kind: err, span }),
			})
	}
}
