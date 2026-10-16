pub mod node_sets;
pub mod nodes;
mod var;
mod rule_error;
mod cover;

use lalrpop_util::{lalrpop_mod, ParseError};

use node_sets::*;

pub use var::Var;
pub use rule_error::{RuleErrorKind, RuleError};
use crate::compiler::{lexer::{Token, TokenTag}, source::Sourced};

lalrpop_mod!(
	#[allow(clippy::all, unused_parens)]
	grammar,
	"/compiler/parser/grammar.rs"
);

pub fn parse(tokens: &[Sourced<Token>]) -> Result<Sourced<Expr>, RuleError> {
	let stream = tokens.iter().map(|token| {
		let range = token.source();
		Ok::<_, RuleError>((range.begin.byte_index(), (**token).clone(), range.end.byte_index()))
	});

	grammar::FileParser::new()
		.parse(stream)
		.map_err(|err| match err {
			ParseError::InvalidToken { location } | ParseError::UnrecognizedEof { location, .. } => RuleError {
				kind: RuleErrorKind::StreamExhausted,
				source_range: Some(cover::range(location, location)),
			},
			ParseError::UnrecognizedToken { token: (l, token, r), expected } => RuleError {
				kind: RuleErrorKind::ExpectedOneOf {
					expected,
					found: TokenTag::from(&token),
				},
				source_range: Some(cover::range(l, r)),
			},
			ParseError::ExtraToken { token: (l, token, r) } => RuleError {
				kind: RuleErrorKind::UnexpectedToken(TokenTag::from(&token)),
				source_range: Some(cover::range(l, r)),
			},
			ParseError::User { error } => error,
		})
}

#[cfg(test)]
mod tests {
	use itertools::Itertools;

	use crate::compiler::{lexer, source::Document};

	use super::{node_sets::*, parse, RuleErrorKind};

	fn parse_str(text: &str) -> Result<String, RuleErrorKind> {
		let document = Document::new("test".to_string(), text.to_string());
		let Ok(tokens) = lexer::lex(&document) else { panic!("failed to lex {:?}", text) };
		parse(&tokens).map(|ast| expr(&ast)).map_err(|err| err.kind)
	}

	fn opt(e: &Option<Box<crate::compiler::source::Sourced<Expr>>>) -> String {
		e.as_ref().map_or("-".to_string(), |e| expr(e))
	}

	fn expr(e: &Expr) -> String {
		match e {
			Expr::Assign(n) => format!("(= {} {} {})", n.op.map_or("-".to_string(), |op| op.to_string()), pattern(&n.lhs), expr(&n.rhs)),
			Expr::Return(n) => format!("(ret {})", opt(&n.expr)),
			Expr::Break(n) => format!("(break {})", opt(&n.expr)),
			Expr::Continue => "continue".to_string(),
			Expr::Unreachable => "unreachable".to_string(),
			Expr::Plex(n) => format!("(plex {})", n.fields.iter().map(|f| format!("{}:{}", f.name, expr(&f.r#type))).join(" ")),
			Expr::Loop(n) => format!("(loop {})", expr(&n.body)),
			Expr::While(n) => format!("(while {} {} {})", expr(&n.cond), expr(&n.body), opt(&n.body_else)),
			Expr::For(n) => format!("(for {} {} {} {})", pattern(&n.binding), expr(&n.iter), expr(&n.body), opt(&n.body_else)),
			Expr::If(n) => format!("(if {} {} {})", expr(&n.cond), expr(&n.body), opt(&n.body_else)),
			Expr::Block(n) => format!("{{{}}}", n.stmts.iter().map(|s| expr(s)).chain([opt(&n.expr)]).join(" ")),
			Expr::Stmt(n) => format!("{};", expr(&n.expr)),
			Expr::BinOp(n) => format!("({} {} {})", n.op, expr(&n.lhs), expr(&n.rhs)),
			Expr::UnaOp(n) => format!("({} {})", n.op, expr(&n.expr)),
			Expr::FieldAccess(n) => format!("(. {} {})", expr(&n.expr), n.ident.name),
			Expr::Func(n) => format!("(fn {} {})", pattern(&n.binding), expr(&n.body)),
			Expr::Call(n) => format!("(call {} {})", expr(&n.func), expr(&n.arg)),
			Expr::Cast(n) => format!("(# {} {})", expr(&n.expr), *n.ty),
			Expr::Extern(n) => format!("(extern {} {})", n.binding.borrow().name, *n.ty),
			Expr::TupleCtor(n) => format!("[{}]", n.items.iter().map(|i| expr(i)).join(" ")),
			Expr::BoolLit(v) => v.to_string(),
			Expr::IntLit(v) => v.to_string(),
			Expr::RealLit(v) => v.to_string(),
			Expr::StrLit(v) => format!("{:?}", v),
			Expr::Identifier(v) => v.borrow().name.clone(),
		}
	}

	fn pattern(p: &Pattern) -> String {
		match p {
			Pattern::Let(n) => format!("(let {})", pattern(&n.pat)),
			Pattern::Mut(n) => format!("(mut {})", pattern(&n.pat)),
			Pattern::TupleDtor(n) => format!("[{}]", n.items.iter().map(|i| pattern(i)).join(" ")),
			Pattern::Deref(e) => format!("(deref {})", expr(e)),
			Pattern::Binding(v) => v.borrow().name.clone(),
			Pattern::Discard => "_".to_string(),
		}
	}

	#[track_caller]
	fn check(text: &str, expected: &str) {
		let expected = [expected, "-"].into_iter().filter(|s| !s.is_empty()).join(" ");
		assert_eq!(parse_str(text).unwrap(), format!("{{{}}}", expected));
	}

	#[test]
	fn file_statements() {
		check("", "");
		check("a; b", "a; b");
		check("let x = 1;", "(= - (let x) 1);");
	}

	#[test]
	fn bin_op_precedence_and_associativity() {
		check("1 - 2 - 3;", "(- (- 1 2) 3);");
		check("1 + 2 * 3;", "(+ 1 (* 2 3));");
		check("1 + 2 << 3;", "(+ 1 (<< 2 3));");
		check("a || b && c == d + e;", "(|| a (&& b (== c (+ d e))));");
		check("a +% b *% c;", "(%+ a (%* b c));");
	}

	#[test]
	fn una_ops() {
		check("-a.*;", "(* (- a));");
		check("a.*.*;", "(* (* a));");
		check("i&;", "(& i);");
		check("a.b.c;", "(. (. a b) c);");
		check("x#u8 + 1;", "(+ (# x u8) 1);");
		check("-x#i8;", "(# (- x) i8);");
		check("x.*#f32#b32;", "(# (# (* x) f32) b32);");
		check("x#(u8, (i8));", "(# x (u8, i8));");
		check("x#(u8,);", "(# x (u8,));");
		check("x#();", "(# x ());");
	}

	#[test]
	fn calls_and_tuples() {
		check("f();", "(call f []);");
		check("f(x,);", "(call f [x]);");
		check("f(x, y,)(z, w);", "(call (call f [x y]) [z w]);");
		check("(1);", "1;");
		check("(1,);", "[1];");
		check("(1, 2, 3,);", "[1 2 3];");
		assert!(parse_str("f(x);").is_err());
	}

	#[test]
	fn assignment_patterns() {
		check("a = b;", "(= - a b);");
		check("a += 1;", "(= + a 1);");
		check("a <<= 1;", "(= << a 1);");
		check("a.* = 1;", "(= - (deref a) 1);");
		check("let mut (a, _) = c;", "(= - (let (mut [a _])) c);");
		check("(let a, let mut b, c) = d;", "(= - [(let a) (let (mut b)) c] d);");
		check("let (x) = y;", "(= - (let [x]) y);");
		assert!(matches!(parse_str("a + b = c;"), Err(RuleErrorKind::ExprInPattern)));
		assert!(matches!(parse_str("(let a, b);"), Err(RuleErrorKind::PatternInExpr)));
	}

	#[test]
	fn control_flow() {
		check("if a: b else c;", "(if a b c);");
		check("if a { b } else c;", "(if a {b} c);");
		check("if a: ret;", "(if a (ret -) -);");
		check("if a: x = 1 else 2;", "(if a (= - x 1) 2);");
		check("if a: while b: c else d;", "(if a (while b c d) -);");
		check("if a: if b: c else d;", "(if a (if b c d) -);");
		check("x = 1 + if a {2} else {3};", "(= - x (+ 1 (if a {2} {3})));");
		check("while a { b; };", "(while a {b; -} -);");
		check("for a in b: c;", "(for a b c -);");
		check("loop { break break continue; };", "(loop {(break (break continue)); -});");
		check("unreachable;", "unreachable;");
	}

	#[test]
	fn block_statements_skip_semicolon() {
		check("if a { b } c", "(if a {b} -); c");
		check("{ while a { b } -c }", "{(while a {b} -); (- c)}");
		check("loop { a } if a { b } else if c { d } e", "(loop {a}); (if a {b} (if c {d} -)); e");
		check("{ if a { b } }", "{(if a {b} -)}");
		assert!(parse_str("if a: b c").is_err());
		assert!(parse_str("x = 1 y = 2").is_err());
	}

	#[test]
	fn functions_and_plex() {
		check("let f = fn(x): x * 3;", "(= - (let f) (fn [x] (* x 3)));");
		check("let f = fn(mut a, b) { a.* - b.* };", "(= - (let f) (fn [(mut a) b] {(- (* a) (* b))}));");
		check("plex { a: x b: y };", "(plex a:x b:y);");
	}

	#[test]
	fn externs() {
		check("extern printf: fn(str, ...) -> i32;", "(extern printf fn(str, ...) -> i32);");
		check("extern exit: fn(i32);", "(extern exit fn(i32));");
		check("extern f: fn();", "(extern f fn());");
		check("extern f: fn(...);", "(extern f fn(...));");
		check("extern f: fn(u8, (i8, f32),) -> fn() -> b1;", "(extern f fn(u8, (i8, f32)) -> fn() -> b1);");
		check("extern @\"weird name\": fn();", "(extern weird name fn());");
		check("x#fn(u8) -> u8;", "(# x fn(u8) -> u8);");
		assert!(parse_str("extern f: fn(..., u8);").is_err()); // `...` has to come last
		assert!(parse_str("extern f;").is_err());
		assert!(parse_str("let x = extern f: fn();").is_err()); // it's a statement, not an expression
	}
}
