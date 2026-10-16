mod token;
mod lexer_error;

use logos::Logos;

pub use lexer_error::LexerError;
pub use token::{Token, TokenTag};

use crate::compiler::{lexer::lexer_error::LexerErrorKind, source::{self, DocumentMeta, Sourced}};

pub fn lex(document: &source::Document) -> Result<Vec<Sourced<Token>>, LexerError> {
	let mut tokens = Vec::new();
	let text = document.get_text();
	let mut lexer = Token::lexer(text);
	while let Some(result) = lexer.next() {
		let byte_span = lexer.span();
		let range = source::Range {
			begin: source::Pos::new_todo_remove_me(byte_span.start),
			end: source::Pos::new_todo_remove_me(byte_span.end),
		};
		
		match result {
			Ok(token) => {
				tokens.push(Sourced::new(token, range));
			},
			Err(mut kind) => {
				let document = DocumentMeta::new(document);
				let range = range.with_meta(&document);
				
				if kind == LexerErrorKind::NoTokenMatchingStream {
					let rest = &text[byte_span.start..];
					if rest.starts_with('"') || rest.starts_with("@\"") {
						kind = LexerErrorKind::StringDelimiterNotClosed;
					}
				}

				let span = range.char_index_range();
				return Err(LexerError { kind, span, tokens });
			},
		}
	}

	let eof = document.eof();
	tokens.push(Sourced::new(Token::Eof, source::Range { begin: eof, end: eof }));
	Ok(tokens)
}

#[cfg(test)]
mod tests {
	use crate::compiler::source::Document;

	use super::{lex, Token};

	fn lex_str(text: &str) -> Option<Vec<Token>> {
		let document = Document::new("test".to_string(), text.to_string());
		lex(&document).ok().map(|tokens| tokens.into_iter().map(|t| t.inner()).collect())
	}

	#[test]
	fn block_comments() {
		assert_eq!(lex_str("a /* b */ c"), Some(vec![Token::Identifier("a".to_string()), Token::Identifier("c".to_string()), Token::Eof]));
		assert_eq!(lex_str("/* a /* b */ c */ d"), Some(vec![Token::Identifier("d".to_string()), Token::Eof]));
		assert_eq!(lex_str("/* a //b */ c"), Some(vec![Token::Identifier("c".to_string()), Token::Eof]));
		assert_eq!(lex_str("/*/ a */"), Some(vec![Token::Eof]));
		assert_eq!(lex_str("/* ░▒▓█ */"), Some(vec![Token::Eof]));
		assert_eq!(lex_str("/* a"), None);
		assert_eq!(lex_str("/* a /* b */"), None);
	}
}
