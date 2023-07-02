use core::slice::Iter;
use std::{fmt::Display, error::Error};
use error_stack::{Report, Result};
use super::{
	lexer::{Token, TokenKind},
	stream::Stream,
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

pub fn ast(source: &str, tokens: &Vec<Token>) -> Result<Node, AstError> {
	let mut stream = Stream::new(tokens.iter());
	match parse_file(&mut stream) {
		Ok(t) => Ok(t),
		Err(err) => Err(err.attach_printable({
			let pos = &stream.get_current().unwrap().source;
			let pos_string = format!("[{}] ", pos);
			let line = pos.get_line(source);
			format!("{:>6$}|\n{}|{}\n{:>6$}|{:>7$}{:^>8$}here", "", pos_string, line, "", "", "", pos_string.len(), pos.begin.index, pos.get_length())
		})),
	}
}

fn parse_file(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	let mut block = Block::new();
	loop {
		discard_space(stream);
		match stream.next() {
			Some(token) => {
				discard_space(stream);
				match token.kind {
					TokenKind::Eof => return Ok(Node { kind: NodeKind::Block(block) }),
					TokenKind::Let => {
						block.stmts.push(parse_let(stream)?);
						discard_space(stream);
					},
					_ => return Err(Report::new(AstError{}).attach_printable(format!("Unexpected token '{}'", token.kind.as_ref()))),
				}
			},
			None => return Err(Report::new(AstError{}).attach_printable("Token stream was exhausted".to_string())),
		};
	};
}

fn parse_let(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	let lhs = Box::new(expr(stream)?);

	discard_space(stream);
	stream_expect_token_kind(stream, TokenKind::Equals)?;

	discard_space(stream);
	let rhs = Box::new(expr(stream)?);

	stream_expect_token_kind(stream, TokenKind::Semicolon)?;
	
	Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })
}

fn parse_block(stream: &mut Stream<Iter<Token>>) -> Result<Node, AstError> {
	let mut block = Block::new();
	loop {
		match stream.next() {
			Some(token) => match token.kind {
				TokenKind::RBrace => break Ok(Node { kind: NodeKind::Block(block) }),
				TokenKind::Let => {
					discard_space(stream);
					block.stmts.push(parse_let(stream)?);
				},
				_ => break Err(Report::new(AstError{}).attach_printable(format!("Unexpected token '{}'", token.kind.as_ref()))),
			},
			None => break Err(Report::new(AstError{}).attach_printable("Block not closed".to_string())),
		};
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
		//TokenKind::Equals => Some(BinOpKind::Assign()),
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
	match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Node { kind: NodeKind::IntLit(*value) }),
			TokenKind::StrLit(value) => Ok(Node { kind: NodeKind::StrLit(value.clone()) }),
			TokenKind::Identifier(value) => Ok(Node { kind: NodeKind::Indentifier(value.clone()) }),
			TokenKind::LBrace => {
				discard_space(stream);
				parse_block(stream)
			}
			_ => Err(Report::new(AstError{}).attach_printable(format!("Unexpected token '{}'", token.kind.as_ref()))),
		},
		None => Err(Report::new(AstError{}).attach_printable("Token stream was exhausted".to_string())),
	}
}
