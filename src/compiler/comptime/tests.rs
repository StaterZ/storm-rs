use crate::compiler::{lexer, parser, semantic_analyzer, source::Document};

use super::{evaluate, Evaluated, EvalErrorKind::{self, *}};
use crate::compiler::semantic_analyzer::Type;

const EXTERNS: &str = "extern printf: fn(str, ...) -> i32; extern scanf: fn(str, ...) -> i32; extern g: fn(i64) -> i64;\n";

fn eval(text: &str) -> Result<Evaluated, Vec<EvalErrorKind>> {
	let text = format!("{}{}", EXTERNS, text);
	let document = Document::new("test".to_string(), text.clone());
	let Ok(tokens) = lexer::lex(&document) else { panic!("failed to lex {:?}", text) };
	let Ok(mut ast) = parser::parse(&tokens) else { panic!("failed to parse {:?}", text) };
	if let Err(errors) = semantic_analyzer::analyze(&mut ast) {
		panic!("failed the semantic analysis of {:?}: {:?}", text, errors.into_iter().map(|err| err.kind).collect::<Vec<_>>());
	}
	evaluate(&ast).map_err(|errors| errors.into_iter().map(|err| err.kind).collect())
}

/// The residual program as a tree, without colors.
fn residual(text: &str) -> String {
	let evaluated = eval(text).unwrap_or_else(|errors| panic!("{:?} failed: {:?}", text, errors));
	let tree = tree_printer::Tree::new(&evaluated.ast).to_string();
	let mut plain = String::new();
	let mut chars = tree.chars();
	while let Some(c) = chars.next() {
		if c == '\x1b' {
			for c in chars.by_ref() {
				if c == 'm' {
					break;
				}
			}
		} else {
			plain.push(c);
		}
	}
	plain
}

#[track_caller]
fn value(text: &str, expected: &str) {
	match eval(text) {
		Ok(evaluated) => assert_eq!(evaluated.last_value.as_deref(), Some(expected), "value of {:?}", text),
		Err(errors) => panic!("{:?} failed: {:?}", text, errors),
	}
}

#[track_caller]
fn err(text: &str, expected: EvalErrorKind) {
	match eval(text) {
		Ok(_) => panic!("{:?} should fail with {:?}", text, expected),
		Err(errors) => assert_eq!(errors, vec![expected], "errors of {:?}", text),
	}
}

fn no_fit(value: &str, ty: Type) -> EvalErrorKind {
	DoesNotFit { value: value.to_string(), ty }
}

fn u8() -> Type {
	Type::Int { signed: false, bits: 8 }
}

#[test]
fn infinite_precision() {
	value("1 + 2", "3");
	value("18446744073709551616 * 2", "36893488147419103232");
	value("-7 / 2", "-3");
	value("-7 % 2", "-1");
	value("(1.0 / 3) * 3", "1.0");
	value("1.0 / 3", "1/3");
	value("1 / 2", "0"); // ints stay ints
	value("0.1 + 0.2 == 0.3", "true"); // no rounding
	value("1 < 2 & 2.5 > 2", "true");
	err("1 / 0", DivisionByZero);
	err("let a = 0; 1 % a", DivisionByZero);
}

#[test]
fn sized_types() {
	value("200u8 +% 100", "44u8");
	value("250u8 + 5", "255u8");
	err("250u8 + 6", no_fit("256", u8()));
	err("1u8 - 2", no_fit("-1", u8()));
	value("-128i8", "-128i8");
	err("-(-128i8)", no_fit("128", Type::Int { signed: true, bits: 8 }));
	value("127i8 *% 2", "-2i8");
	value("1u7 + 126", "127u7");
	value("0.1f32 + 0.2f32", "0.30000001192092896f32"); // exactly like runtime f32
	value("0.1f64 + 0.2f64", "0.30000000000000004f64");
	value("1.5f32 * 2", "3.0f32");
}

#[test]
fn casts() {
	value("300#i64#u8", "44u8");
	value("(-1)#i8#b8", "0xff (b8)");
	value("1.5f32#b32", "0x3fc00000 (b32)");
	value("0x3fc00000#f32", "1.5f32");
	value("2.9#i64", "2i64");
	value("-2.9#i64", "-2i64");
	value("(1, 2.5)#(u8, f32)", "(1u8, 2.5f32)");
	err("(255 + 1)#u8", no_fit("256", u8()));
}

#[test]
fn bools() {
	// unsized bits keep every bit until they meet a size
	value("0xf0 | 0x0f", "0xff");
	value("0d2 | 3", "0x3");
	value("0xf0 & 0x3c", "0x30");
	value("0xf0 ^ 0xff", "0xf");
	value("0b0001 << 70", "0x400000000000000000");
	value("0b1000 >> 3", "true");
	value("(0xf0 | 0x0f)#b8", "0xff (b8)");
	value("0xf0 | 3#b8", "0xf3 (b8)");
	value("!(0x0f#b8)", "0xf0 (b8)");
	value("0x3fc00000#f32", "1.5f32");
	value("0x41#u8", "65u8");
	err("!0x0f", UnsizedNot { value: "0xf".to_string() });
	err("(0x100 | 0xff)#b8", no_fit("511", Type::Bool(8)));
	err("(0x100 | 0xff)#u8", no_fit("511", u8()));
	// a single bit is a plain bool
	value("true ^ true", "false");
	value("true & false | true", "true");
	value("!true", "false");
	err("let b = 0x0f; let _ = printf(\"%d\", b,);", UnsizedAtRuntime { value: "0xf".to_string() });
	assert!(eval("let b = 0x0f#b8; let _ = printf(\"%d\", b,);").is_ok());
	err("1#b8 << 8", ShiftTooFar { amount: "8".to_string(), ty: Type::Bool(8) });
}

#[test]
fn variables_and_control_flow() {
	value("let a = 5; let b = a * 2; b + 1", "11");
	value("let mut i = 0; let mut s = 0; while i < 10 { i += 1; s += i; }; s", "55");
	value("let mut i = 0; loop { i += 1; if i == 7 { break; }; }; i", "7");
	value("let x = loop { break 5; }; x", "5");
	value("let mut n = 0; let mut i = 0; while i < 10 { i += 1; if i % 2 == 0 { continue; }; n += 1; }; n", "5");
	value("let mut r = 0; while false { } else { r = 1; }; r", "1");
	value("if 1 > 2 { 1 } else { 2 }", "2");
	value("let a = { let b = 2; b * 3 }; a", "6");
	value("let t = (1, (2, 3)); let (a, (b, c)) = t; a + b * c", "7");
	value("let mut (a, b) = (1, 2); (a, b) = (b, a); a * 10 + b", "21");
	value("let mut big = 1; let mut i = 0; while i < 100 { big *= 2; i += 1; }; big", "1267650600228229401496703205376");
}

#[test]
fn functions() {
	value("let f = fn(a, b) { a * b }; f(6, 7,)", "42");
	value("let f = fn(x): x * 3; f(f(2,),)", "18");
	value("let f = fn(x) { loop { ret x; }; }; f(9,)", "9");
	value("let f = fn t: t; f(1, 2,)", "(1, 2)");
	value("let fact = fn(n) { let mut r = 1; let mut i = 1; while i <= n { r *= i; i += 1; }; r }; fact(25,)", "15511210043330985984000000");
}

#[test]
fn pointers() {
	value("let mut x = 1#i64; let p = x.&; p.* = 5; x", "5i64");
	value("let mut x = 1#i64; let p = x.&; p.* += 2; p.*", "3i64");
	value("let f = fn(p) { p.* += 1; }; let mut x = 1; f(x.&,); f(x.&,); x", "3");
}

#[test]
fn runtime_needs_a_size() {
	err("let mut i = 0; let _ = scanf(\"%d\", i.&,);", NeedsRuntimeType { name: "i".to_string(), ty: Type::ComptimeInt });
	err("let big = 18446744073709551616; let _ = printf(\"%d\", big,);", TooLargeForRuntime { value: "18446744073709551616".to_string() });
	// a comptime value that fits the default i64 can go to runtime as it is
	assert!(eval("let _ = g(5,);").is_ok());
	assert!(eval("let mut i = 0#i64; let _ = scanf(\"%d\", i.&,);").is_ok());
}

#[test]
fn residual_program() {
	// everything known at compile time leaves nothing behind but the externs
	let tree = residual("let f = fn(x): x * 3; let a = f(14,);");
	assert!(!tree.contains("rhs: (Func)") && !tree.contains("(Call)"), "{}", tree);

	// a call that needs runtime stays, with its known arguments filled in
	let tree = residual("let f = fn(x): x * 3; let _ = g(f(14,),);");
	assert!(tree.contains("42 (IntLit)") && !tree.contains("rhs: (Func)"), "{}", tree);

	// a loop with runtime effects runs at runtime, with its variable declared from where it was known
	let tree = residual("let mut i = 0#i64; while i < 3 { let _ = printf(\"x\",); i += 1; };");
	assert!(tree.contains("(While)") && tree.contains("i (") && tree.contains("0i64 (IntLit)"), "{}", tree);

	// a runtime condition keeps both branches, and what they change lives at runtime
	let tree = residual("let mut a = 1#i64; if g(0,) == 0 { a = 2; }; let _ = g(a,);");
	assert!(tree.contains("(If)") && tree.contains("1i64 (IntLit)"), "{}", tree);

	// a function only used at runtime is kept, with its known parts folded
	let tree = residual("let f = fn(x) { x + 2 * 3 }; let _ = f(g(1,),);");
	assert!(tree.contains("rhs: (Func)") && tree.contains("6 (IntLit)"), "{}", tree);

	// an infinite loop can't finish at compile time, so it's left for runtime
	let tree = residual("let mut i = 0#i64; loop { i += 1; };");
	assert!(tree.contains("(Loop)"), "{}", tree);
}
