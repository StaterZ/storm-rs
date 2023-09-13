use core::slice::Iter;
use error_stack::Report;
use color_print::cformat;
use super::{
	lexer::{Token, TokenKind},
	stream::{Stream, StreamExt},
	source_meta::SourceFile,
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
pub use thiserror;

mod node;

#[derive(Debug, thiserror::Error)]
pub enum AstError {
	#[error("Hard")]
	Hard,
	#[error("Soft")]
	Soft,
}

macro_rules! try_hard {
	($expr:expr) => {
		match $expr {
			Ok(ok) => Ok(ok),
			Err(err) => match err.current_context() {
				AstError::Hard => return Err(err),
				AstError::Soft => Err(err),
			}
		}
	};
}

fn discard_space(stream: &mut Stream<Iter<Token>>) {
	while stream.expect(|&t| matches!(t.kind,
		| TokenKind::Space
		| TokenKind::NewLine
		| TokenKind::Comment
		| TokenKind::MultilineComment
	)).is_some() { };
}

fn stream_expect_token_kind<'a>(stream: &'a mut Stream<Iter<Token>>, kind: TokenKind) -> Result<&'a Token, String> {
	stream.expect_err(|&t| if t.kind == kind {
		Ok(())
	} else {
		Err(format!("Expected token '{}' but found '{}'", kind.as_ref(), t.kind.as_ref()))
	})
}

pub fn ast<'a>(source: &'a SourceFile, tokens: &'a Vec<Token>) -> Result<Node, Report<AstError>> {
	let mut stream = tokens.iter().stream();
	match parse_file(&mut stream) {
		Ok(t) => Ok(t),
		Err(err) => Err(err.attach_printable({
			let source_range = &stream.get_current().unwrap().source;
			let source_range_string = format!("[{}]", source_range.with_source(source));
			let line = source_range.get_line(source);
			let line_trunc_length = line
				.char_indices()
				.find_map(|(i, c)| (!matches!(c, '\t' | ' ')).then_some(i))
				.unwrap_or(0);
			let line = &line[line_trunc_length..];

			cformat!(
				"<cyan>{empty:>source_range_string_len$}--></><green>{file_path}</>\n\
				 <cyan>{empty:>source_range_string_len$} | </>\n\
				 <cyan>{source_range_string            } | </>{line}\n\
				 <cyan>{empty:>source_range_string_len$} | </><red>{empty:>error_inset$}{empty:^>error_length$}here</>",
				empty = "",
				source_range_string = source_range_string,
				source_range_string_len = source_range_string.len(),
				file_path = source.get_path().display(),
				line = line,
				error_inset = source_range.begin.with_meta(source).unwrap().column0() - line_trunc_length,
				error_length = source_range.get_length().unwrap(),
			)
		})),
	}
}

fn parse_file(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
	let mut block = Block::new();
	loop {
		discard_space(stream);

		if stream.expect(|&t| t.kind == TokenKind::Eof).is_some() {
			return Ok(Node { kind: NodeKind::Block(block) });
		}
		
		if let Ok(stmt) = try_hard!(parse_stmt(stream)) {
			block.stmts.push(stmt);
			continue;
		}

		return match stream.get_current() {
			Some(token) => Err(Report::new(AstError::Soft)
				.attach_printable(format!("Failed to parse file. token: '{}'", token.kind.as_ref()))),
			None => Err(Report::new(AstError::Soft)
				.attach_printable("Token stream was exhausted".to_string())),
		};
	};
}

fn parse_stmt(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
	if let Ok(stmt) = try_hard!(parse_let(stream)) {
		return Ok(stmt);
	}
	if let Ok(stmt) = try_hard!(parse_assignment(stream)) {
		return Ok(stmt);
	}
	if let Ok(stmt) = try_hard!(parse_block(stream)) {
		return Ok(stmt);
	}

	return Err(Report::new(AstError::Soft)
		.attach_printable("Failed to parse stmt"));
}

fn parse_let(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
	if let Err(err) = stream_expect_token_kind(stream, TokenKind::Let) {
		return Err(Report::new(AstError::Soft).attach_printable(err));
	}
	
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
	stream_expect_token_kind(stream, TokenKind::Semicolon)
		.map_err(|err| Report::new(AstError::Hard)
			.attach_printable(err))?;
	
	Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })
}

fn parse_assignment(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
	let lhs = match try_hard!(expr(stream)) {
		Ok(lhs) => Box::new(lhs),
		Err(err) => return Err(Report::new(AstError::Soft).attach_printable(err)),
	};
	
	discard_space(stream);
	let rhs = if stream.expect(|c| c.kind == TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(expr(stream)?))
	} else {
		None
	};
	
	discard_space(stream);
	stream_expect_token_kind(stream, TokenKind::Semicolon)
		.map_err(|err| Report::new(AstError::Hard)
			.attach_printable(err))?;
	
	Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })
}

fn parse_block(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
	stream_expect_token_kind(stream, TokenKind::LBrace)
		.map_err(|err| Report::new(AstError::Soft)
			.attach_printable(err))?;
	
	let mut block = Block::new();
	loop {
		discard_space(stream);

		if stream.expect(|&t| t.kind == TokenKind::RParen).is_some() {
			return Ok(Node { kind: NodeKind::Block(block) });
		}
		
		if let Ok(stmt) = try_hard!(parse_stmt(stream)) {
			block.stmts.push(stmt);
			continue;
		}

		return match stream.get_current() {
			Some(token) => Err(Report::new(AstError::Soft)
				.attach_printable(format!("Failed to parse block. token: '{}'", token.kind.as_ref()))),
			None => Err(Report::new(AstError::Soft)
				.attach_printable("Token stream was exhausted".to_string())),
		};
	};
}

fn expr(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
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

fn expr_atom(stream: &mut Stream<Iter<Token>>) -> Result<Node, Report<AstError>> {
	stream.hypothetically(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Node { kind: NodeKind::IntLit(*value) }),
			TokenKind::StrLit(value) => Ok(Node { kind: NodeKind::StrLit(value.clone()) }),
			TokenKind::Identifier(value) => Ok(Node { kind: NodeKind::Indentifier(value.clone()) }),
			_ => Err(Report::new(AstError::Soft)
				.attach_printable("Trying to parse atom")
				.attach_printable(format!("Unexpected token '{}'", token.kind.as_ref()))),
		},
		None => Err(Report::new(AstError::Soft)
			.attach_printable("Token stream was exhausted".to_string())),
	})
}
