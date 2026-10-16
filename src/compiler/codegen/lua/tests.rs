use mlua::Lua;

use crate::compiler::{lexer, parser, semantic_analyzer, source::Document};

use super::generate;

fn compile(text: &str) -> String {
	let document = Document::new("test".to_string(), text.to_string());
	let Ok(tokens) = lexer::lex(&document) else { panic!("failed to lex {:?}", text) };
	let Ok(mut ast) = parser::parse(&tokens) else { panic!("failed to parse {:?}", text) };
	if let Err(errors) = semantic_analyzer::analyze(&mut ast) {
		panic!("failed the semantic analysis of {:?}: {:?}", text, errors.into_iter().map(|err| err.kind).collect::<Vec<_>>());
	}
	let Ok(code) = generate(&ast) else { panic!("failed to generate lua for {:?}", text) };
	String::from_utf8(code).unwrap()
}

const EXTERNS: &str = "extern printf: fn(str, ...) -> i32; extern scanf: fn(str, ...) -> i32;\n";

#[derive(Debug)]
struct Run {
	output: String,
	exit_code: Option<i64>,
}

/// Runs a program with `input` as the lines of stdin, catching what it writes and its exit code.
fn run_with_input(text: &str, input: &[&str]) -> Result<Run, String> {
	// snippets get the C functions the data files declare themselves
	let text = if text.contains("extern ") { text.to_string() } else { format!("{}{}", EXTERNS, text) };
	let code = compile(&text);
	let lua = Lua::new();
	lua.load(r#"
		__out = {}
		io.write = function(...) for _, s in ipairs({...}) do __out[#__out + 1] = tostring(s) end end
		os.exit = function(code) __exit_code = code end
		io.read = function() return table.remove(__input, 1) end
	"#).exec().unwrap();
	let lua_input = lua.create_table().unwrap();
	for (i, line) in input.iter().enumerate() {
		let value: mlua::Value = lua.load(*line).eval().unwrap();
		lua_input.set(i + 1, value).unwrap();
	}
	lua.globals().set("__input", lua_input).unwrap();

	lua.load(&code).exec().map_err(|err| format!("{}\n--- generated lua ---\n{}", err, code))?;
	let output: String = lua.load("table.concat(__out)").eval().unwrap();
	let exit_code: Option<i64> = lua.globals().get("__exit_code").unwrap();
	Ok(Run { output, exit_code })
}

fn run(text: &str) -> Result<Run, String> {
	run_with_input(text, &[])
}

#[track_caller]
fn exits_with(text: &str, expected: i64) {
	match run(text) {
		Ok(run) => assert_eq!(run.exit_code, Some(expected), "exit code of {:?}", text),
		Err(err) => panic!("{:?} failed to run: {}", text, err),
	}
}

#[track_caller]
fn prints(text: &str, expected: &str) {
	match run(text) {
		Ok(run) => assert_eq!(run.output, expected, "output of {:?}", text),
		Err(err) => panic!("{:?} failed to run: {}", text, err),
	}
}

#[test]
fn test_storm() {
	let text = std::fs::read_to_string("data/test.storm").unwrap();
	let run = run_with_input(&text, &["5"]).unwrap();
	assert_eq!(run.output, "give number: hello world! 15\n");
	assert_eq!(run.exit_code, Some(42));
}

#[test]
fn data_files_load() {
	// some of these loop forever, so they're only checked to be valid lua
	for path in ["data/a.storm", "data/b.storm", "data/c.storm", "data/sallad.storm", "data/test.storm"] {
		let code = compile(&std::fs::read_to_string(path).unwrap());
		if let Err(err) = Lua::new().load(&code).into_function() {
			panic!("{} generated invalid lua: {}\n--- generated lua ---\n{}", path, err, code);
		}
	}
}

#[test]
fn arithmetic() {
	exits_with("ret 1 - 2 - 3 + 10;", 6);
	exits_with("ret 2 + 3 * 4;", 14);
	exits_with("ret (2 + 3) * 4;", 20);
	exits_with("ret 7 / 2;", 3);
	exits_with("ret -7 / 2;", -3); // rounds toward zero like C
	exits_with("ret 7 % 3;", 1);
	exits_with("ret -7 % 3;", -1);
	exits_with("ret 1#b8 << 4;", 16);
	exits_with("ret -(2 + 3);", -5);
	exits_with("ret +5;", 5);
	prints("let _ = printf(\"%.2f\", 1.5 * 2 + 0.25,);", "3.25");
	prints("let _ = printf(\"%s %s\", 1 < 2, \"ok\",);", "true ok");
}

#[test]
fn bools() {
	exits_with("let t = true && 1 < 2; ret if t { 1 } else { 0 };", 1);
	exits_with("ret if !false || false { 1 } else { 0 };", 1);
	exits_with("ret !(5#b8)#i8;", -6); // `!` on a bool type flips its bits
}

#[test]
fn casts() {
	exits_with("let x = 300#i64; ret x#u8;", 44);
	exits_with("let x = -1#i64; ret x#u8;", 255);
	exits_with("let x = 200#u8; ret x#i8;", -56);
	exits_with("ret 2.9#i64;", 2);
	exits_with("ret -2.9#i64;", -2);
	exits_with("ret 1.5f32#b32;", 0x3fc00000);
	prints("let _ = printf(\"%s\", 3#f64,);", "3.0");
	prints("let _ = printf(\"%s %s\", 0#b1, 1#b1,);", "false true");
}

#[test]
fn control_flow() {
	exits_with("let a = if 1 < 2 { 10 } else { 20 }; ret a;", 10);
	exits_with("let a = if 1 > 2 { 10 } else if 2 > 3 { 20 } else { 30 }; ret a;", 30);
	exits_with("let mut i = 0#i64; let a = loop { i += 1; if i == 5 { break i * 2; }; }; ret a;", 10);
	exits_with("let mut i = 0#i64; let mut sum = 0#i64; while i < 10 { i += 1; if i % 2 == 0 { continue; }; sum += i; }; ret sum;", 25);
	exits_with("let mut i = 0#i64; let mut r = 0#i64; while i < 3 { i += 1; } else { r = 7; }; ret r;", 7);
	exits_with("let mut r = 0#i64; while true { break; } else { r = 7; }; ret r;", 0);
	exits_with("let mut i = 0#i64; loop { loop { break; }; i += 1; if i == 3 { break; }; }; ret i;", 3);
	exits_with("let a = 1; let b = { let a = 2; a * 10 }; ret a + b;", 21);
	exits_with("let a = { ret 9; }; ret 1;", 9);
	assert!(run("unreachable;").unwrap_err().contains("unreachable"));
}

#[test]
fn functions() {
	exits_with("let add = fn(a, b): a + b; ret add(2, 3,);", 5);
	exits_with("let f = fn(x) { loop { ret 7; }; }; ret f(1,);", 7);
	exits_with("let f = fn(x): x * 3; ret f(f(2,),);", 18);
	exits_with("let f = fn(_, b): b; ret f(1, 2,);", 2);
	exits_with("let f = fn t: t; let (a, b) = f(1, 2,); ret b;", 2); // a single binding gets the whole tuple
	exits_with("let f = fn((a, b), c): a + b + c; ret f((1, 2), 3,);", 6);
	exits_with("ret (fn(x): x + 1)(1,);", 2);
	prints("let f = fn() { let _ = printf(\"hi\",); }; f();", "hi");
}

#[test]
fn bindings() {
	exits_with("let mut (a, b) = (1#i64, 2#i64); (a, b) = (b, a); ret a * 10 + b;", 21);
	exits_with("let t = (1, (2, 3)); let (a, (b, c)) = t; ret a + b * c;", 7);
	exits_with("let (a, _, c) = (1, 2, 3); ret a + c;", 4);
	exits_with("let end = 1; let @\"local\" = 2; let __x = 3; ret end + @\"local\" + __x;", 6);
	exits_with("let mut x = 1#i64; let p = x&; p.* = 5; p.* += 1; ret x;", 6);
	exits_with("let mut x = 1#i64; let p = x&; let q = p.*&; q.* = 3; ret x;", 3);
	exits_with("let mut n = 0#i64; let _ = false && { n += 1; true }; let _ = true || { n += 1; true }; ret n;", 0);
	exits_with("let mut n = 0#i64; let _ = true && { n += 1; true }; ret n;", 1);
}

#[test]
fn evaluation_order() {
	// the call runs before the `if`, even though the `if` needs statements first
	prints("let f = fn(s) { let _ = printf(s,); 1 }; let _ = f(\"a\",) + if true { let _ = f(\"b\",); 1 } else { 2 };", "ab");
	prints("let f = fn(s) { let _ = printf(s,); 1 }; let _ = (f(\"a\",), { let _ = f(\"b\",); 1 }, f(\"c\",));", "abc");
}

#[test]
fn externs() {
	prints("extern print: fn(str); print(\"from lua\",);", ""); // lua's own print writes to stdout, not io.write
	assert!(run("extern nope: fn(); nope();").unwrap_err().contains("unknown extern 'nope'"));
	exits_with("extern tostring: fn(i64) -> str; extern tonumber: fn(str) -> i64; ret tonumber(tostring(41,),) + 1;", 42);
}

#[test]
fn scanf() {
	let run = run_with_input("let mut a = 0#i64; let mut b = 0#i64; let _ = scanf(\"%d %d\", a&, b&,); ret a - b;", &["10", "3"]).unwrap();
	assert_eq!(run.exit_code, Some(7));
}
