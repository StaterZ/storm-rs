mod token;
mod lexer_error;
pub mod literal;

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

	Ok(tokens)
}

#[cfg(test)]
mod tests {
	use crate::compiler::source::Document;

	use super::{lex, lexer_error::LexerErrorKind, literal::parse_num, Token};

	fn lex_str(text: &str) -> Option<Vec<Token>> {
		lex_result(text).ok()
	}

	fn lex_result(text: &str) -> Result<Vec<Token>, LexerErrorKind> {
		let document = Document::new("test".to_string(), text.to_string());
		lex(&document)
			.map(|tokens| tokens.into_iter().map(|t| t.inner()).collect())
			.map_err(|err| err.kind)
	}

	fn num(text: &str) -> Token {
		Token::NumLit(parse_num(text).unwrap())
	}

	fn ident(name: &str) -> Token {
		Token::Identifier(name.to_string())
	}

	fn str(value: &str) -> Token {
		Token::StrLit(value.to_string())
	}

	#[test]
	fn number_tokens() {
		assert_eq!(lex_str("42"), Some(vec![num("42")]));
		assert_eq!(lex_str("1.5"), Some(vec![num("1.5")]));
		assert_eq!(lex_str("1e-3"), Some(vec![num("1e-3")]));
		assert_eq!(lex_str("1.5e+3f32"), Some(vec![num("1.5e3f32")]));
		assert_eq!(lex_str("1e-3 + 2"), Some(vec![num("1e-3"), Token::Plus, num("2")]));
		// only decimal literals take a signed exponent, so this stays a subtraction
		assert_eq!(lex_str("0x1e-3"), Some(vec![num("0x1e"), Token::Dash, num("3")]));
		assert_eq!(lex_str("1 - 3"), Some(vec![num("1"), Token::Dash, num("3")]));
		assert_eq!(lex_str("1-3"), Some(vec![num("1"), Token::Dash, num("3")]));
		assert_eq!(lex_str("x1e-3"), Some(vec![ident("x1e"), Token::Dash, num("3")]));
		// a `.` is only part of a number when a digit follows it
		assert_eq!(lex_str("1.foo"), Some(vec![num("1"), Token::Dot, ident("foo")]));
		assert_eq!(lex_str("1.*"), Some(vec![num("1"), Token::Deref]));
		assert_eq!(lex_str("1..2"), Some(vec![num("1"), Token::Dot, Token::Dot, num("2")]));
		assert_eq!(lex_str("1.5.2"), Some(vec![num("1.5"), Token::Dot, num("2")]));
		assert_eq!(lex_str("-128i8"), Some(vec![Token::Dash, num("128i8")]));
		assert_eq!(lex_str("f(1u8, 2.0f64,)"), Some(vec![
			ident("f"), Token::LParen, num("1u8"), Token::Comma, num("2.0f64"), Token::Comma, Token::RParen,
		]));
	}

	#[test]
	fn number_token_errors() {
		assert_eq!(lex_result("1e - 3"), Err(LexerErrorKind::ExponentHasNoDigits));
		assert_eq!(lex_result("1e-3x"), Err(LexerErrorKind::InvalidDigitForRadix { digit_symbol: 'x', digit_value: 33, radix: 10 }));
		assert_eq!(lex_result("256u8"), Err(LexerErrorKind::NumDoesNotFit { value: 256u32.into(), ty: super::literal::IntType::Unsigned(8) }));
		assert_eq!(lex_result("1.5u8"), Err(LexerErrorKind::RealWithIntSuffix(super::literal::IntType::Unsigned(8))));
	}

	#[test]
	fn string_tokens() {
		assert_eq!(lex_str(r#""""#), Some(vec![str("")]));
		assert_eq!(lex_str(r#""abc""#), Some(vec![str("abc")]));
		assert_eq!(lex_str(r#""a b  c""#), Some(vec![str("a b  c")]));
		assert_eq!(lex_str(r#""❤️n! ░▒▓█""#), Some(vec![str("❤️n! ░▒▓█")]));
		assert_eq!(lex_str("\"a\nb\""), Some(vec![str("a\nb")]));
		assert_eq!(lex_str(r#""\n\t\r\\\"\'\0""#), Some(vec![str("\n\t\r\\\"'\0")]));
		assert_eq!(lex_str(r#""\b\f\v\/""#), Some(vec![str("\u{8}\u{c}\u{b}/")]));
		assert_eq!(lex_str(r#""\x41\u{1F600}é\101""#), Some(vec![str("A😀éA")]));
		assert_eq!(lex_str(r#""a\"b""#), Some(vec![str("a\"b")]));
		assert_eq!(lex_str(r#""// not a comment""#), Some(vec![str("// not a comment")]));
		assert_eq!(lex_str(r#""/* not a comment */""#), Some(vec![str("/* not a comment */")]));
		assert_eq!(lex_str(r#""a""b""#), Some(vec![str("a"), str("b")]));
		assert_eq!(lex_str(r#"f("%d\n", x,)"#), Some(vec![
			ident("f"), Token::LParen, str("%d\n"), Token::Comma, ident("x"), Token::Comma, Token::RParen,
		]));
	}

	#[test]
	fn string_token_errors() {
		assert_eq!(lex_result(r#""abc"#), Err(LexerErrorKind::StringDelimiterNotClosed));
		assert_eq!(lex_result(r#""abc\""#), Err(LexerErrorKind::StringDelimiterNotClosed));
		assert_eq!(lex_result(r#"x = ""#), Err(LexerErrorKind::StringDelimiterNotClosed));
		assert!(matches!(lex_result(r#""\q""#), Err(LexerErrorKind::InvalidEscapeSequence(_))));
		assert!(matches!(lex_result(r#""\u{110000}""#), Err(LexerErrorKind::InvalidEscapeSequence(_))));
		assert!(matches!(lex_result(r#""\xZZ""#), Err(LexerErrorKind::InvalidEscapeSequence(_))));
	}

	#[test]
	fn identifier_tokens() {
		assert_eq!(lex_str("abc _a a1 été"), Some(vec![ident("abc"), ident("_a"), ident("a1"), ident("été")]));
		assert_eq!(lex_str("_"), Some(vec![Token::Discard]));
		assert_eq!(lex_str("let lets"), Some(vec![Token::Let, ident("lets")]));
		assert_eq!(lex_str(r#"@"hello world""#), Some(vec![ident("hello world")]));
		assert_eq!(lex_str(r#"@"let""#), Some(vec![ident("let")]));
		assert_eq!(lex_str(r#"@"a\"b\n""#), Some(vec![ident("a\"b\n")]));
		assert_eq!(lex_result(r#"@"abc"#), Err(LexerErrorKind::StringDelimiterNotClosed));
		assert!(matches!(lex_result(r#"@"\q""#), Err(LexerErrorKind::InvalidEscapeSequence(_))));
	}

	#[test]
	fn block_comments() {
		assert_eq!(lex_str("a /* b */ c"), Some(vec![Token::Identifier("a".to_string()), Token::Identifier("c".to_string())]));
		assert_eq!(lex_str("/* a /* b */ c */ d"), Some(vec![Token::Identifier("d".to_string())]));
		assert_eq!(lex_str("/* a //b */ c"), Some(vec![Token::Identifier("c".to_string())]));
		assert_eq!(lex_str("/*/ a */"), Some(vec![]));
		assert_eq!(lex_str("/* ░▒▓█ */"), Some(vec![]));
		assert_eq!(lex_str("/* a"), None);
		assert_eq!(lex_str("/* a /* b */"), None);
	}

	#[test]
	fn line_comments() {
		let c = || Token::Identifier("c".to_string());
		assert_eq!(lex_str("// a\nc"), Some(vec![c()]));
		assert_eq!(lex_str("// a\r\nc"), Some(vec![c()]));
		assert_eq!(lex_str("// a\rc"), Some(vec![c()]));
		assert_eq!(lex_str("// a"), Some(vec![]));
	}
}
