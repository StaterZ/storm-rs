use core::slice::Iter;
use std::{fmt::Display, error::Error};
use error_stack::{Report, Result, ResultExt};
use color_print::cformat;
use super::{
	lexer::{Token, TokenKind},
	stream::{Stream, StreamExt},
};
pub use self::node::{
	NodeKind,
	Node,
	Block,
	Let,
	BinOp,
	MathBinOpKind,
	MathBinOpVariant,
	BinOpKind,
	CmpBinOpKind
};

mod node;

#[derive(Debug)]
pub struct AstError {}

impl Display for AstError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Abstract Syntax Tree Error")
	}
}

impl Error for AstError {}

fn discard_space(stream: &mut Stream<Iter<Token>>) {
	while stream.expect(|&t| matches!(t.kind,
		| TokenKind::Space
		| TokenKind::NewLine
		| TokenKind::Comment
		| TokenKind::MultilineComment
	)).is_some() { };
}

fn stream_expect_token_kind<'a>(stream: &'a mut Stream<Iter<Token>>, kind: TokenKind) -> Result<&'a Token, AstError> {
	stream.expect_err(|&t| if t.kind == kind {
		Ok(())
	} else {
		Err(format!("Expected token '{}' but found '{}'", kind.as_ref(), t.kind.as_ref()))
	}).map_err(|err| Report::new(AstError{}).attach_printable(err))
}

pub fn ast<'a>(source: &'a str, tokens: &'a Vec<Token>) -> Result<Node, AstError> {
	let mut stream = tokens.iter().stream();
	match parse_file(&mut stream) {
		Ok(t) => Ok(t),
		Err(err) => Err(err.attach_printable({
			let file_path = "/filePathGoesHere.storm";

			let pos = &stream.get_current().unwrap().source;
			let pos_string = format!("[{}]", pos);
			let line = pos.get_line(source);
			let line_trunc_length = line
				.chars()
				.position(|c| !matches!(c, '\t' | ' '))
				.unwrap_or(0);
			let line = &line[line_trunc_length..];

			cformat!(
				"<cyan>{empty:>pos_string_len$}--></><green>{file_path}</>\n\
				 <cyan>{empty:>pos_string_len$} | </>\n\
				 <cyan>{pos_string            } | </>{line}\n\
				 <cyan>{empty:>pos_string_len$} | </><red>{empty:>error_inset$}{empty:^>error_length$}here</>",
				empty = "",
				pos_string = pos_string,
				pos_string_len = pos_string.len(),
				file_path = file_path,
				line = line,
				error_inset = pos.begin.column0() - line_trunc_length,
				error_length = pos.get_length(),
			)
		})),
	}
}

fn parse_file(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	let mut block = Block::new();
	loop {
		discard_space(stream);
		match stream.get_current() {
			Some(token) => {
				match token.kind {
					TokenKind::Eof => {
						stream.next();
						return Ok(Node { kind: NodeKind::Block(block) })
					},
					TokenKind::Let => {
						block.stmts.push(parse_let(stream)?);
					},
					TokenKind::LBrace => {
						stream.next();
						discard_space(stream);
						block.stmts.push(parse_block(stream)?);
					},
					_ => return Err(Report::new(AstError{})
						.attach_printable(format!("Unexpected token '{}'", token.kind.as_ref()))),
				}
			},
			None => return Err(Report::new(AstError{})
				.attach_printable("Token stream was exhausted".to_string())),
		};
	};
}

fn parse_let(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	stream_expect_token_kind(stream, TokenKind::Let)?;
	
	discard_space(stream);
	let lhs = Box::new(expr(stream)?);
	
	discard_space(stream);
	let rhs = if stream.expect(|c| c.kind == TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(expr(stream)?))
	} else {
		None
	};
	
	discard_space(stream);
	stream_expect_token_kind(stream, TokenKind::Semicolon)?;
	
	Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })
}

fn parse_block(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	stream_expect_token_kind(stream, TokenKind::LBrace)?;
	discard_space(stream);

	let mut block = Block::new();
	loop {
		if stream.hypothetically(|stream| match stream.get_current() {
			Some(token) => match token.kind {
				TokenKind::RBrace => {
					stream.next();
					Ok(true)
				},
				TokenKind::Let => {
					block.stmts.push(parse_let(stream)?);
					Ok(false)
				},
				_ => Err(Report::new(AstError{})
					.attach_printable(format!("Unexpected token '{}'", token.kind.as_ref())))
					.attach_printable("Trying to parse statement"),
			},
			None => Err(Report::new(AstError{})
				.attach_printable("Block not closed".to_string())),
		})? {
			return Ok(Node { kind: NodeKind::Block(block) });
		}
		discard_space(stream);
	}
}

fn expr(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	let lhs = expr_atom(stream)?;
	
	let mut stream = stream.dup();
	discard_space(stream.get());
	let op = stream.get().expect_map(|&t| match t.kind {
		TokenKind::Plus => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Add, allow_wrap: false })),
		TokenKind::Dash => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Sub, allow_wrap: false })),
		TokenKind::Star => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Mul, allow_wrap: false })),
		TokenKind::Slash => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Div, allow_wrap: false })),
		TokenKind::Percent => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Mod, allow_wrap: false })),
		TokenKind::LShift => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Shl, allow_wrap: false })),
		TokenKind::RShift => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Shr, allow_wrap: false })),
		//TokenKind::Equals => Some(BinOpKind::Assign(None)),
		TokenKind::Equality => Some(BinOpKind::Cmp(CmpBinOpKind { lt: false, eq: true, gt: false })),
		_ => None,
	});

	Ok(match op {
		None => {
			stream.pop();
			lhs
		},
		Some((_, op)) => {
			discard_space(stream.get());
			let mut rhs = expr(stream.get())?;
			stream.nip();

			if let NodeKind::BinOp(rhs_bin_op) = &rhs.kind {
				if rhs_bin_op.op < op {
					let mut rhs_bin_op = rhs.kind.into_bin_op().unwrap();

					rhs_bin_op.lhs = Box::new(Node{
						kind: NodeKind::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: rhs_bin_op.lhs,
						}),
					});
					rhs.kind = NodeKind::BinOp(rhs_bin_op);
					rhs
				} else {
					Node {
						kind: NodeKind::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: Box::new(rhs),
						}
					)}
				}
			} else {
				Node {
					kind: NodeKind::BinOp(BinOp {
						op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					}
				)}
			}
		},
	})
}

fn expr_atom(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	stream.hypothetically(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Node { kind: NodeKind::IntLit(*value) }),
			TokenKind::StrLit(value) => Ok(Node { kind: NodeKind::StrLit(value.clone()) }),
			TokenKind::Identifier(value) => Ok(Node { kind: NodeKind::Indentifier(value.clone()) }),
			TokenKind::LBrace => {
				discard_space(stream);
				parse_block(stream)
			}
			/*TokenKind::Let => {
				discard_space(stream);
				parse_let(stream)
			},*/
			_ => Err(Report::new(AstError{})
				.attach_printable(format!("Unexpected token '{}'", token.kind.as_ref()))
				.attach_printable("Trying to parse atom")),
		},
		None => Err(Report::new(AstError{})
			.attach_printable("Token stream was exhausted".to_string())),
	})
}
