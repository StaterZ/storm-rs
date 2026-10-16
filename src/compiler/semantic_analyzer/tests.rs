use crate::compiler::{lexer, parser, source::Document};

use super::{analyze, Type, SemErrorKind::{self, *}};

fn check_str(text: &str) -> Result<Type, Vec<SemErrorKind>> {
	let document = Document::new("test".to_string(), text.to_string());
	let Ok(tokens) = lexer::lex(&document) else { panic!("failed to lex {:?}", text) };
	let Ok(mut ast) = parser::parse(&tokens) else { panic!("failed to parse {:?}", text) };
	analyze(&mut ast).map_err(|errors| errors.into_iter().map(|err| err.kind).collect())
}

#[track_caller]
fn ok(text: &str) {
	if let Err(errors) = check_str(text) {
		panic!("{:?} should type check, but got {:?}", text, errors);
	}
}

#[track_caller]
fn err(text: &str, expected: SemErrorKind) {
	match check_str(text) {
		Ok(_) => panic!("{:?} should fail with {:?}", text, expected),
		Err(errors) => assert_eq!(errors, vec![expected], "errors of {:?}", text),
	}
}

fn int(signed: bool, bits: u64) -> Type {
	Type::Int { signed, bits }
}

fn mismatch(expected: Type, found: Type) -> SemErrorKind {
	Mismatch { expected, found }
}

fn no_fit(value: &str, ty: Type) -> SemErrorKind {
	LiteralDoesNotFit { value: value.to_string(), ty }
}

#[test]
fn type_names() {
	assert_eq!(Type::from_name("i32"), Some(int(true, 32)));
	assert_eq!(Type::from_name("u7"), Some(int(false, 7)));
	assert_eq!(Type::from_name("i0"), Some(int(true, 0)));
	assert_eq!(Type::from_name("u1000000"), Some(int(false, 1000000)));
	assert_eq!(Type::from_name("f32"), Some(Type::Float(32)));
	assert_eq!(Type::from_name("b1"), Some(Type::Bool(1)));
	assert_eq!(Type::from_name("usize"), Some(Type::Usize));
	assert_eq!(Type::from_name("isize"), Some(Type::Isize));
	assert_eq!(Type::from_name("str"), Some(Type::Str));
	assert_eq!(Type::from_name("f0"), None);
	assert_eq!(Type::from_name("b0"), None);
	assert_eq!(Type::from_name("i08"), None);
	assert_eq!(Type::from_name("i"), None);
	assert_eq!(Type::from_name("x32"), None);
	assert_eq!(Type::from_name("IntLit"), None); // comptime types can't be named
}

#[test]
fn type_display() {
	assert_eq!(int(true, 42).to_string(), "i42");
	assert_eq!(Type::VOID.to_string(), "VOID");
	assert_eq!(Type::Nothing.to_string(), "NOTHING");
	assert_eq!(Type::Tuple(vec![Type::Bool(1)]).to_string(), "(b1,)");
	assert_eq!(Type::Tuple(vec![Type::IntLit, Type::Str]).to_string(), "(IntLit, str)");
	assert_eq!(Type::Ptr(Box::new(int(false, 8))).to_string(), "u8&");
	assert_eq!(Type::Func { arg: Box::new(Type::Tuple(vec![Type::Unknown])), ret: Box::new(Type::Float(32)) }.to_string(), "fn(unknown,) -> f32");
}

#[test]
fn comptime_literals() {
	ok("let a = 1; let b = 1.5; let c = true; let d = \"s\";");
	ok("let a = 1u8 + 2;");
	ok("let a = 2 + 1u8;");
	ok("let a = 1 + 1.5;");
	ok("let a = 1.5f32 + 1;");
	ok("let a = 1.5f32 * 2.5;");
	ok("let a = 18446744073709551616 + 1;"); // comptime ints don't have a size
	err("let a = 1u8 + 300;", no_fit("300", int(false, 8)));
	err("let a = -1 + 1u8;", no_fit("-1", int(false, 8)));
	ok("let a = -128 + 1i8;");
	err("let a = -129 + 1i8;", no_fit("-129", int(true, 8)));
	err("let a = 128 + 1i8;", no_fit("128", int(true, 8)));
	err("let a = 1.5 + 1u8;", InvalidBinOp { op: "+".to_string(), lhs: Type::RealLit, rhs: int(false, 8) });
	err("let a = true + 1;", InvalidBinOp { op: "+".to_string(), lhs: Type::BoolLit, rhs: Type::IntLit });
}

#[test]
fn implicit_casts() {
	ok("let mut a = 0#b32; a = 5;");
	ok("let mut a = 0#usize; a = 5;");
	ok("let mut a = 0#f64; a = 5; a = 1.5;");
	ok("let mut a = 0#b1; a = true;");
	err("let mut a = 0#b8; a = 300;", no_fit("300", Type::Bool(8)));
	err("let mut a = 0#u8; a = 1.5;", mismatch(int(false, 8), Type::RealLit));
	err("let mut a = 0#u8; a = 1i8;", mismatch(int(false, 8), int(true, 8)));
	err("let mut a = 0#u8; a = true;", mismatch(int(false, 8), Type::BoolLit));
	ok("let mut a = 0#u8; a += 1;");
	err("let mut a = 0#u8; a += 256;", no_fit("256", int(false, 8)));
	err("let mut a = 0#u8; a += 1.5;", InvalidBinOp { op: "+".to_string(), lhs: int(false, 8), rhs: Type::RealLit });
}

#[test]
fn explicit_casts() {
	ok("let a = 1u8#i32;");
	ok("let a = 1#u8;");
	ok("let a = 1.5#i32;");
	ok("let a = 1i64#f32;");
	ok("let a = 1#usize#u8;");
	ok("let a = 1i8#b8;");
	ok("let a = 1.5f32#b32#f32;");
	ok("let a = 1#b16#b8;");
	ok("let a = (1, 2.5)#(u8, f32);");
	ok("let mut a = 0#u8; let b = a&#usize;");
	err("let a = 300#u8;", no_fit("300", int(false, 8)));
	err("let a = -1#u8;", no_fit("-1", int(false, 8)));
	err("let a = 1i8#b16;", InvalidCast { from: int(true, 8), to: Type::Bool(16) });
	err("let a = 1.5f32#b8;", InvalidCast { from: Type::Float(32), to: Type::Bool(8) });
	err("let a = \"s\"#u8;", InvalidCast { from: Type::Str, to: int(false, 8) });
	err("let a = 1#nope;", UnknownType("nope".to_string()));
	err("let a = 1#f0;", UnknownType("f0".to_string()));
	err("let a = (1, 2)#(u8,);", InvalidCast { from: Type::Tuple(vec![Type::IntLit, Type::IntLit]), to: Type::Tuple(vec![int(false, 8)]) });
}

#[test]
fn operators() {
	ok("let a = 1#b8 << 2;");
	ok("let a = 1#b8 >> 2u8;");
	err("let a = 1u8 << 2;", ShiftNonBool(int(false, 8)));
	err("let a = 1 << 2;", ShiftNonBool(Type::IntLit));
	err("let a = 1#b8 << 1.5;", InvalidBinOp { op: "<<".to_string(), lhs: Type::Bool(8), rhs: Type::RealLit });
	err("let a = 1#b8 + 1#b8;", InvalidBinOp { op: "+".to_string(), lhs: Type::Bool(8), rhs: Type::Bool(8) });
	ok("let a = true && 1 == 2 || 1u8 < 2;");
	ok("let a = \"a\" == \"b\";");
	err("let a = 1u8 && true;", InvalidBinOp { op: "&&".to_string(), lhs: int(false, 8), rhs: Type::BoolLit });
	err("let a = 1u8 == 1i8;", InvalidBinOp { op: "==".to_string(), lhs: int(false, 8), rhs: int(true, 8) });
	err("let a = true < false;", InvalidBinOp { op: "<".to_string(), lhs: Type::BoolLit, rhs: Type::BoolLit });
	ok("let a = !true; let b = !(1#b8); let c = -1i8; let d = -1.5; let e = +1u8;");
	err("let a = !1u8;", InvalidUnaOp { op: "!".to_string(), ty: int(false, 8) });
	err("let a = -(1u8);", InvalidUnaOp { op: "-".to_string(), ty: int(false, 8) });
	err("let a = -(1#usize);", InvalidUnaOp { op: "-".to_string(), ty: Type::Usize });
}

#[test]
fn control_flow() {
	ok("if true { };");
	ok("if 1 == 2: ret;");
	err("if 1u8 { };", mismatch(Type::Bool(1), int(false, 8)));
	err("while 1.5 { };", mismatch(Type::Bool(1), Type::RealLit));
	ok("while 1 { };"); // IntLit casts to b1
	ok("let a = if true { 1u8 } else { 2 };");
	ok("let a = if true { 1 } else { 2.5 };");
	err("let a = if true { 1u8 } else { 2i8 };", mismatch(int(false, 8), int(true, 8)));
	err("if true { 1u8 };", UncaughtValue(int(false, 8)));
	ok("let a = if true { ret 1; } else { 2u8 };"); // ret never gives a value
	ok("let a = loop { break 1u8; };");
	ok("let a = loop { if true { break 1u8; }; break 2; };");
	err("let a = loop { if true { break 1u8; }; break 2i8; };", mismatch(int(false, 8), int(true, 8)));
	err("while true { break 1; };", BreakValueInWhile(Type::IntLit));
	ok("while true { break; };");
	ok("loop { while true { continue; }; break; };");
	err("break;", BreakOutsideLoop);
	err("continue;", BreakOutsideLoop);
	err("loop { let f = fn(x) { break; }; };", BreakOutsideLoop);
}

#[test]
fn cascading() {
	ok("let a = 1;");
	ok("{ let a = 1; };");
	ok("let a = { 1u8 };");
	err("1u8;", UncaughtValue(int(false, 8)));
	err("(1, 2);", UncaughtValue(Type::Tuple(vec![Type::IntLit, Type::IntLit])));
	err("let f = fn(x) { 1u8 }; f(1,);", UncaughtValue(int(false, 8)));
	ok("let f = fn(x) { 1u8 }; let _ = f(1,);");
	ok("let f = fn(x) { x; }; f(1,);"); // NOTHING doesn't need to be caught
	err("{ 1u8 };", UncaughtValue(int(false, 8)));
	err("while true { 1u8 };", UncaughtValue(int(false, 8)));
	err("loop { 1u8 };", UncaughtValue(int(false, 8)));
	ok("();"); // the empty tuple is VOID
	ok("printf(\"a\",);"); // printf isn't typed yet
}

#[test]
fn bindings() {
	ok("let a = 1; let b = a + 1u8;");
	err("let a = 1i8; let b = a + 1u8;", InvalidBinOp { op: "+".to_string(), lhs: int(true, 8), rhs: int(false, 8) });
	ok("let mut a = 1u8;");
	err("let mut a = 1;", MutComptime(Type::IntLit));
	err("let mut a = 1.5;", MutComptime(Type::RealLit));
	err("let mut a = true;", MutComptime(Type::BoolLit));
	ok("let (a, b) = (1, 2u8);");
	ok("let (a, (b, c)) = (1, (2u8, 3));");
	ok("let mut (a, b) = (1u8, 2#i32);");
	err("let mut (a, b) = (1u8, 2);", MutComptime(Type::IntLit));
	err("let (a, b) = (1, 2, 3);", DestructureMismatch { found: Type::Tuple(vec![Type::IntLit; 3]), len: 2 });
	err("let (a, b) = 1;", DestructureMismatch { found: Type::IntLit, len: 2 });
	ok("let mut a = 1u8; let mut b = 2u8; (a, b) = (b, 3);");
	err("let mut a = 1u8; let mut b = 2u8; (a, b) = (b, 300);", no_fit("300", int(false, 8)));
}

#[test]
fn pointers() {
	ok("let mut a = 1u8; let p = a&; p.* = 2;");
	ok("let mut a = 1u8; let p = a&; let b = p.* + 1;");
	ok("let mut a = 1u8; let p = a&; p.* += 1;");
	err("let mut a = 1u8; let p = a&; p.* = 300;", no_fit("300", int(false, 8)));
	err("let mut a = 1u8; let p = a&; p.* = 1i8;", mismatch(int(false, 8), int(true, 8)));
	err("let a = 1u8.*;", NotAPointer(int(false, 8)));
	err("let a = 1u8; a.* = 1;", NotAPointer(int(false, 8)));
}

#[test]
fn functions() {
	ok("let f = fn(x): x * 3; let y = f(1,);");
	ok("let f = fn(mut a, mut b) { a += 1; a.* - b.* };");
	ok("let f = fn(x) { ret 1; };");
	err("let a = 1u8(2,);", NotCallable(int(false, 8)));
	err("let a = \"s\"(2,);", NotCallable(Type::Str));
	err("let a = 1u8; let b = a.field;", NoFields(int(false, 8)));
	ok("let f = fn(x): x.field;"); // unknown types might have fields
}

#[test]
fn reports_every_error() {
	assert_eq!(check_str("let a = 300#u8; let b = 1u8 << 1;"), Err(vec![
		no_fit("300", int(false, 8)),
		ShiftNonBool(int(false, 8)),
	]));
}

#[test]
fn symbols() {
	ok("let a = 1; let b = a;");
	ok("let mut a = 1u8; a = 2;");
	ok("let a = 1; let a = a + 1;"); // shadowing
	ok("let f = fn(x) { printf(\"%d\", x,); };");
	err("let a = b;", UndefinedSymbol("b".to_string()));
	err("a = 1;", UndefinedSymbol("a".to_string()));
	err("let a = 1u8; a = 2;", AssignImmutableSymbol("a".to_string()));
	err("let a = 1u8; a += 2;", AssignImmutableSymbol("a".to_string()));
	err("let let a = 1;", DoubleLet);
	err("let mut mut a = 1u8;", DoubleMut);
	err("mut a = 1u8;", MutWithoutLet);
	err("let a += 1;", CompoundAssignDeclaration);
	err("_ += 1;", CompoundAssignDeclaration);
}

#[test]
fn scopes() {
	err("{ let a = 1; }; let b = a;", UndefinedSymbol("a".to_string()));
	err("if true { let a = 1; }; let b = a;", UndefinedSymbol("a".to_string()));
	err("while true { let a = 1; }; let b = a;", UndefinedSymbol("a".to_string()));
	err("let b = 1; for a in b { }; let c = a;", UndefinedSymbol("a".to_string()));
	err("let a = 1; let f = fn(x): a;", UndefinedSymbol("a".to_string())); // functions don't capture
	ok("let a = { let b = 1u8; b + 1 };"); // a block's final expression sees its symbols
}

#[test]
fn last_statement_type() {
	assert_eq!(check_str(""), Ok(Type::Nothing));
	assert_eq!(check_str("1u8 + 2"), Ok(int(false, 8)));
	assert_eq!(check_str("let a = 1; a + 1.5"), Ok(Type::RealLit));
	assert_eq!(check_str("let a = 1;"), Ok(Type::Nothing));
	assert_eq!(check_str("(1, 2.5)#(u8, f32)"), Ok(Type::Tuple(vec![int(false, 8), Type::Float(32)])));
	assert_eq!(check_str("let a = 1u8; a&"), Ok(Type::Ptr(Box::new(int(false, 8)))));
	assert_eq!(check_str("1 < 2"), Ok(Type::Bool(1)));
	assert_eq!(check_str("fn(x): 1u8"), Ok(Type::Func { arg: Box::new(Type::Tuple(vec![Type::Unknown])), ret: Box::new(int(false, 8)) }));
}
