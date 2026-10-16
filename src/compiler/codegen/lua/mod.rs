#[cfg(test)]
mod tests;

use std::{cell::RefCell, mem, ops::Deref, rc::Rc};

use itertools::Itertools;

use crate::compiler::{parser::{node_sets::*, nodes::*, Var}, source::Sourced};

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	Unsupported(&'static str),
}

/// Helpers the generated code relies on. `printf` and `scanf` stand in for the C functions the LLVM backend links.
const PRELUDE: &str = r#"local __write = io.write
local function printf(fmt, ...)
	local s = string.format(fmt, ...)
	__write(s)
	return #s
end
local function scanf(fmt, ...)
	local refs = {...}
	local count = 0
	for spec in fmt:gmatch("%%(%a)") do
		local ref = refs[count + 1]
		if ref == nil then break end
		local value
		if spec == "s" then
			value = io.read("l")
		else
			value = io.read("n")
			if value ~= nil and spec ~= "f" and spec ~= "g" and spec ~= "e" then value = math.tointeger(value) end
		end
		if value == nil then break end
		ref.set(value)
		count = count + 1
	end
	return count
end
local function __div(a, b)
	if math.type(a) == "integer" and math.type(b) == "integer" then
		local q = a // b
		if a % b ~= 0 and (a < 0) ~= (b < 0) then q = q + 1 end
		return q
	end
	return a / b
end
local function __mod(a, b)
	return math.fmod(a, b)
end
local function __not(a)
	if type(a) == "boolean" then return not a end
	return ~a
end
local function __ref(value)
	return { get = function() return value end, set = function(v) value = v end }
end
local function __wrap(x, bits, signed)
	if bits >= 64 then return x end
	if bits == 0 then return 0 end
	x = x & ((1 << bits) - 1)
	if signed and x >= (1 << (bits - 1)) then x = x - (1 << bits) end
	return x
end
local function __int(x, bits, signed)
	if type(x) == "boolean" then x = x and 1 or 0 end
	if math.type(x) == "float" then x = math.tointeger(x >= 0 and math.floor(x) or math.ceil(x)) or 0 end
	return __wrap(x, bits, signed)
end
local function __float(x)
	if type(x) == "boolean" then return x and 1.0 or 0.0 end
	return x + 0.0
end
local function __bool(x)
	if type(x) == "boolean" then return x end
	return x ~= 0
end
local function __bits(x, bits)
	if type(x) == "boolean" then return x and 1 or 0 end
	if math.type(x) == "float" then
		if bits == 32 then return (string.unpack("<I4", string.pack("<f", x))) end
		if bits == 64 then return (string.unpack("<i8", string.pack("<d", x))) end
		x = math.tointeger(x >= 0 and math.floor(x) or math.ceil(x)) or 0
	end
	return __wrap(x, bits, false)
end
"#;

const KEYWORDS: &[&str] = &[
	"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in",
	"local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
];

/// Turns a program into a Lua chunk. The program's `ret` value becomes the exit code, like with the LLVM backend.
pub fn generate(ast: &Sourced<Expr>) -> Result<Vec<u8>, GenError> {
	let mut lua = Gen {
		lines: Vec::new(),
		depth: 1,
		next_id: 0,
		loops: Vec::new(),
		unsupported: None,
	};
	lua.stmt(ast);
	if let Some(unsupported) = lua.unsupported {
		return Err(GenError::Unsupported(unsupported));
	}

	let mut out = String::from(PRELUDE);
	out.push_str("local __exit = (function(...)\n");
	for line in lua.lines {
		out.push_str(&line);
		out.push('\n');
	}
	out.push_str("end)(...)\nif math.type(__exit) == \"integer\" then os.exit(__exit) end\n");
	Ok(out.into_bytes())
}

struct Loop {
	id: usize,
	/// The local a `loop` stores its `break` value in.
	value: Option<String>,
}

struct Gen {
	lines: Vec<String>,
	depth: usize,
	next_id: usize,
	loops: Vec<Loop>,
	unsupported: Option<&'static str>,
}

/// A Lua name for a Storm symbol. Storm allows names Lua doesn't, like keywords, unicode or `@"a b"`.
fn name(var: &Rc<RefCell<Var>>) -> String {
	let name = var.borrow().name.clone();
	let is_valid = name.chars().next().is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
		&& name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
		&& !name.starts_with("__")
		&& !KEYWORDS.contains(&name.as_str());
	if is_valid {
		name
	} else {
		format!("__n_{}", name.bytes().map(|b| format!("{:02x}", b)).join(""))
	}
}

/// Whether evaluating `code` again or later gives the same result and has no side effects.
fn is_constant(code: &str) -> bool {
	matches!(code, "nil" | "true" | "false") || code.parse::<f64>().is_ok() || (code.starts_with('"') && code.ends_with('"') && code.len() >= 2)
}

/// Lua only allows calls, fields and indexing on names, calls, indexing or parenthesized expressions.
fn prefix_exp(code: String) -> String {
	if is_name(&code) || code.ends_with(')') || code.ends_with(']') {
		code
	} else {
		format!("({})", code)
	}
}

fn is_name(code: &str) -> bool {
	code.chars().next().is_some_and(|c| c.is_ascii_alphabetic() || c == '_') && code.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

impl Gen {
	fn emit(&mut self, line: impl AsRef<str>) {
		let line = line.as_ref();
		// a statement starting with `(` would be read as a call on the line before it
		let sep = if line.starts_with('(') { ";" } else { "" };
		self.lines.push(format!("{}{}{}", "\t".repeat(self.depth), sep, line));
	}

	fn id(&mut self) -> usize {
		self.next_id += 1;
		self.next_id
	}

	fn temp(&mut self) -> String {
		format!("__t{}", self.id())
	}

	/// Stores a value in a local so it's only evaluated once, here.
	fn materialize(&mut self, code: String) -> String {
		if is_constant(&code) {
			return code;
		}
		let temp = self.temp();
		self.emit(format!("local {} = {}", temp, code));
		temp
	}

	/// Runs `f` with the statements it emits collected separately.
	fn capture<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> (Vec<String>, T) {
		let lines = mem::take(&mut self.lines);
		let result = f(self);
		(mem::replace(&mut self.lines, lines), result)
	}

	fn indented<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
		self.depth += 1;
		let result = f(self);
		self.depth -= 1;
		result
	}

	/// Runs `f` inside a `do ... end`, so its locals don't leak.
	fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
		self.emit("do");
		let result = self.indented(f);
		self.emit("end");
		result
	}

	/// The values of several expressions, evaluated in order. A value is stored in a local when a later one has to emit statements first.
	fn values(&mut self, nodes: &[&Sourced<Expr>]) -> Vec<String> {
		let generated = nodes.iter().map(|node| self.capture(|this| this.value(node))).collect_vec();
		let last_with_stmts = generated.iter().rposition(|(lines, _)| !lines.is_empty());
		generated
			.into_iter()
			.enumerate()
			.map(|(i, (lines, code))| {
				self.lines.extend(lines);
				match last_with_stmts {
					Some(last) if i < last => self.materialize(code),
					_ => code,
				}
			})
			.collect()
	}

	/// Generates an expression whose value is dropped.
	fn stmt(&mut self, node: &Sourced<Expr>) {
		match node.deref() {
			Expr::Stmt(stmt) => self.stmt(&stmt.expr),
			Expr::Block(block) => self.scoped(|this| {
				this.block_stmts(block, false);
			}),
			Expr::Assign(assign) => self.assign(assign),
			Expr::If(node_if) => self.if_else(node_if, None),
			Expr::Call(_) => {
				let call = self.value(node);
				self.emit(call);
			},
			_ => {
				let code = self.value(node);
				if !is_constant(&code) && !is_name(&code) {
					self.emit(format!("local _ = {}", code));
				}
			},
		}
	}

	/// Generates a block's statements, and its final expression as a value when `want_value` is set.
	fn block_stmts(&mut self, block: &Block, want_value: bool) -> Option<String> {
		for stmt in block.stmts.iter() {
			self.stmt(stmt);
		}
		match &block.expr {
			Some(expr) if want_value => Some(self.value(expr)),
			Some(expr) => {
				self.stmt(expr);
				None
			},
			None => None,
		}
	}

	/// Generates an expression and gives back Lua code for its value.
	fn value(&mut self, node: &Sourced<Expr>) -> String {
		match node.deref() {
			Expr::Assign(assign) => {
				self.assign(assign);
				"nil".to_string()
			},
			Expr::Return(ret) => {
				let value = ret.expr.as_ref().map(|expr| self.value(expr));
				match value {
					Some(value) => self.emit(format!("do return {} end", value)),
					None => self.emit("do return end"),
				}
				"nil".to_string()
			},
			Expr::Break(brk) => {
				let value = brk.expr.as_ref().map(|expr| self.value(expr));
				match self.loops.last() {
					Some(lp) => {
						let id = lp.id;
						if let (Some(target), Some(value)) = (lp.value.clone(), value) {
							self.emit(format!("{} = {}", target, value));
						}
						self.emit(format!("goto __brk{}", id));
					},
					None => self.emit("error(\"break outside a loop\")"),
				}
				"nil".to_string()
			},
			Expr::Continue => {
				match self.loops.last() {
					Some(lp) => {
						let id = lp.id;
						self.emit(format!("goto __cont{}", id));
					},
					None => self.emit("error(\"continue outside a loop\")"),
				}
				"nil".to_string()
			},
			Expr::Unreachable => {
				self.emit("error(\"unreachable\")");
				"nil".to_string()
			},

			Expr::Plex(_) => {
				self.unsupported = Some("plex");
				"nil".to_string()
			},

			Expr::Loop(lp) => {
				let value = self.temp();
				self.emit(format!("local {}", value));
				self.lp(Some(value.clone()), |this, id| {
					this.emit("while true do");
					this.indented(|this| this.loop_body(&lp.body, id));
					this.emit("end");
				});
				value
			},
			Expr::While(wh) => {
				self.lp(None, |this, id| {
					let (cond_lines, cond) = this.capture(|this| this.value(&wh.cond));
					if cond_lines.is_empty() {
						this.emit(format!("while {} do", cond));
						this.indented(|this| this.loop_body(&wh.body, id));
					} else {
						// the condition needs statements, so it's checked inside the loop
						this.emit("while true do");
						this.indented(|this| {
							this.lines.extend(cond_lines.into_iter().map(|line| format!("\t{}", line)));
							this.emit(format!("if not ({}) then break end", cond));
							this.loop_body(&wh.body, id);
						});
					}
					this.emit("end");
					if let Some(body_else) = &wh.body_else {
						this.scoped(|this| this.stmt(body_else));
					}
				});
				"nil".to_string()
			},
			Expr::For(fr) => {
				let iter = self.value(&fr.iter);
				self.scoped(|this| {
					let iter = this.materialize(iter);
					this.lp(None, |this, id| {
						this.emit("while true do");
						this.indented(|this| {
							let next = this.temp();
							this.emit(format!("local {} = {}.next()", next, iter));
							this.emit(format!("if {} == nil then break end", next));
							this.scoped(|this| {
								this.bind(&fr.binding, next, false);
								this.stmt(&fr.body);
							});
							this.emit(format!("::__cont{}::", id));
						});
						this.emit("end");
						if let Some(body_else) = &fr.body_else {
							this.scoped(|this| this.stmt(body_else));
						}
					});
				});
				"nil".to_string()
			},
			Expr::If(node_if) => match node_if.body_else {
				None => {
					self.if_else(node_if, None);
					"nil".to_string()
				},
				Some(_) => {
					let value = self.temp();
					self.emit(format!("local {}", value));
					self.if_else(node_if, Some(&value));
					value
				},
			},

			Expr::Block(block) => {
				if block.stmts.is_empty() && let Some(expr) = &block.expr {
					return self.value(expr);
				}
				let value = self.temp();
				self.emit(format!("local {}", value));
				self.scoped(|this| {
					if let Some(code) = this.block_stmts(block, true) {
						this.emit(format!("{} = {}", value, code));
					}
				});
				value
			},
			Expr::Stmt(stmt) => {
				self.stmt(&stmt.expr);
				"nil".to_string()
			},
			Expr::BinOp(bin_op) => self.bin_op(bin_op),
			Expr::UnaOp(una_op) => {
				if una_op.op == UnaOpKind::AddressOf {
					return self.address_of(&una_op.expr);
				}
				let expr = self.value(&una_op.expr);
				match una_op.op {
					UnaOpKind::Deref => format!("{}.get()", prefix_exp(expr)),
					UnaOpKind::AddressOf => unreachable!("handled above"),
					UnaOpKind::Identity => expr,
					UnaOpKind::Negate => format!("(-{})", expr),
					UnaOpKind::Not => format!("__not({})", expr),
				}
			},
			Expr::FieldAccess(field_access) => {
				let expr = self.value(&field_access.expr);
				format!("{}.{}", prefix_exp(expr), field_access.ident.name)
			},
			Expr::Func(func) => self.func(func),
			Expr::Call(call) => {
				let func = self.value(&call.func);
				let items = match &**call.arg {
					Expr::TupleCtor(tuple) => tuple.items.iter().collect_vec(),
					_ => vec![&*call.arg],
				};
				let args = self.values(&items);
				format!("{}({})", prefix_exp(func), args.join(", "))
			},
			Expr::Cast(cast) => {
				let expr = self.value(&cast.expr);
				cast_code(expr, &cast.ty)
			},

			Expr::TupleCtor(tuple) => {
				if tuple.items.is_empty() {
					return "nil".to_string(); // VOID
				}
				let items = self.values(&tuple.items.iter().collect_vec());
				format!("{{{}}}", items.join(", "))
			},
			Expr::BoolLit(value) => value.to_string(),
			Expr::IntLit(lit) => lit.value.to_string(),
			Expr::RealLit(lit) => match lit.to_f64() {
				value if value.is_infinite() => "math.huge".to_string(),
				value => format!("{:?}", value),
			},
			Expr::StrLit(value) => format!("\"{}\"", value.escape_default()),
			Expr::Identifier(var) => name(var),
		}
	}

	/// Runs `f` as the body of a loop that `break` and `continue` can jump out of.
	fn lp(&mut self, value: Option<String>, f: impl FnOnce(&mut Self, usize)) {
		let id = self.id();
		self.loops.push(Loop { id, value });
		self.scoped(|this| f(this, id));
		self.loops.pop();
		self.emit(format!("::__brk{}::", id));
	}

	fn loop_body(&mut self, body: &Sourced<Expr>, id: usize) {
		// `continue` jumps past the body's scope, so it never jumps into the scope of a local
		self.scoped(|this| this.stmt(body));
		self.emit(format!("::__cont{}::", id));
	}

	/// An `if`, with its value stored in `value` when there is an `else`.
	fn if_else(&mut self, node_if: &If, value: Option<&str>) {
		let cond = self.value(&node_if.cond);
		self.emit(format!("if {} then", cond));
		self.indented(|this| this.branch(&node_if.body, value));
		if let Some(body_else) = &node_if.body_else {
			self.emit("else");
			self.indented(|this| this.branch(body_else, value));
		}
		self.emit("end");
	}

	fn branch(&mut self, body: &Sourced<Expr>, value: Option<&str>) {
		match value {
			Some(value) => {
				let code = self.value(body);
				self.emit(format!("{} = {}", value, code));
			},
			None => self.stmt(body),
		}
	}

	fn bin_op(&mut self, bin_op: &BinOp) -> String {
		if let BinOpKind::Logic(op) = bin_op.op {
			// the right side only runs when it's needed
			let lhs = self.value(&bin_op.lhs);
			let (rhs_lines, rhs) = self.capture(|this| this.value(&bin_op.rhs));
			if rhs_lines.is_empty() {
				let op = match op {
					LogicBinOpKind::And => "and",
					LogicBinOpKind::Or => "or",
				};
				return format!("({} {} {})", lhs, op, rhs);
			}
			let value = self.temp();
			self.emit(format!("local {} = {}", value, lhs));
			let cond = match op {
				LogicBinOpKind::And => value.clone(),
				LogicBinOpKind::Or => format!("not {}", value),
			};
			self.emit(format!("if {} then", cond));
			self.indented(|this| {
				this.lines.extend(rhs_lines.into_iter().map(|line| format!("\t{}", line)));
				this.emit(format!("{} = {}", value, rhs));
			});
			self.emit("end");
			return value;
		}

		let [lhs, rhs] = self.values(&[&bin_op.lhs, &bin_op.rhs]).try_into().expect("two values for two operands");
		bin_op_code(bin_op.op, &lhs, &rhs)
	}

	fn address_of(&mut self, node: &Sourced<Expr>) -> String {
		match node.deref() {
			Expr::Identifier(var) => {
				let name = name(var);
				format!("{{ get = function() return {} end, set = function(v) {} = v end }}", name, name)
			},
			// `p.*&` is `p`
			Expr::UnaOp(UnaOp { op: UnaOpKind::Deref, expr }) => self.value(expr),
			// anything else points at a copy
			_ => {
				let value = self.value(node);
				format!("__ref({})", value)
			},
		}
	}

	fn func(&mut self, func: &Func) -> String {
		let loops = mem::take(&mut self.loops);
		let depth = self.depth;
		self.depth += 1;
		let (lines, params) = self.capture(|this| {
			let params = this.params(&func.binding);
			let ret = this.value(&func.body);
			if ret != "nil" {
				this.emit(format!("do return {} end", ret));
			}
			params
		});
		self.depth = depth;
		self.loops = loops;

		let mut code = format!("function({})\n", params);
		for line in lines {
			code.push_str(&line);
			code.push('\n');
		}
		code.push_str(&"\t".repeat(self.depth));
		code.push_str("end");
		code
	}

	/// Lua parameters for a function's binding. Functions take one tuple, which is spread into the parameters.
	fn params(&mut self, binding: &Sourced<Pattern>) -> String {
		let Pattern::TupleDtor(dtor) = strip_decl(binding).deref() else {
			// a single binding gets the whole tuple
			self.bind(binding, "{...}".to_string(), true);
			return "...".to_string();
		};
		dtor.items
			.iter()
			.map(|item| match strip_decl(item).deref() {
				Pattern::Binding(var) => name(var),
				Pattern::Discard => "_".to_string(),
				_ => {
					let param = self.temp();
					self.bind(item, param.clone(), true);
					param
				},
			})
			.join(", ")
	}

	fn assign(&mut self, assign: &Assign) {
		if let Some(op) = assign.op {
			// a compound assignment reads and writes the same target
			match strip_decl(&assign.lhs).deref() {
				Pattern::Deref(ptr) => {
					let ptr = self.value(ptr);
					let ptr = self.materialize(ptr);
					let rhs = self.value(&assign.rhs);
					self.emit(format!("{}.set({})", ptr, bin_op_code(op, &format!("{}.get()", ptr), &rhs)));
				},
				Pattern::Binding(var) => {
					let rhs = self.value(&assign.rhs);
					let name = name(var);
					self.emit(format!("{} = {}", name, bin_op_code(op, &name, &rhs)));
				},
				_ => self.emit("error(\"unsupported compound assignment\")"),
			}
			return;
		}

		// destructuring a tuple constructor binds each item directly. every item is evaluated before any is assigned, so `(a, b) = (b, a)` swaps
		if let (Pattern::TupleDtor(dtor), Expr::TupleCtor(tuple)) = (&**strip_decl(&assign.lhs), &**assign.rhs)
			&& dtor.items.len() == tuple.items.len()
			&& !tuple.items.is_empty()
		{
			let values = self.values(&tuple.items.iter().collect_vec())
				.into_iter()
				.map(|value| self.materialize(value))
				.collect_vec();
			let is_decl = is_decl(&assign.lhs);
			for (item, value) in dtor.items.iter().zip(values) {
				self.bind(item, value, is_decl);
			}
			return;
		}

		let value = self.value(&assign.rhs);
		self.bind(&assign.lhs, value, false);
	}

	/// Stores `value` into a pattern, declaring its bindings as locals when `is_decl` is set.
	fn bind(&mut self, pattern: &Sourced<Pattern>, value: String, is_decl: bool) {
		match pattern.deref() {
			Pattern::Let(lt) => self.bind(&lt.pat, value, true),
			Pattern::Mut(mt) => self.bind(&mt.pat, value, is_decl),
			Pattern::Binding(var) => {
				let local = if is_decl { "local " } else { "" };
				self.emit(format!("{}{} = {}", local, name(var), value));
			},
			Pattern::Discard => if !is_constant(&value) && !is_name(&value) {
				self.emit(format!("local _ = {}", value));
			},
			Pattern::Deref(ptr) => {
				let ptr = self.value(ptr);
				self.emit(format!("{}.set({})", prefix_exp(ptr), value));
			},
			Pattern::TupleDtor(dtor) => {
				let tuple = if is_name(&value) { value } else { self.materialize(value) };
				for (i, item) in dtor.items.iter().enumerate() {
					self.bind(item, format!("{}[{}]", tuple, i + 1), is_decl);
				}
			},
		}
	}
}

fn strip_decl(pattern: &Sourced<Pattern>) -> &Sourced<Pattern> {
	match pattern.deref() {
		Pattern::Let(lt) => strip_decl(&lt.pat),
		Pattern::Mut(mt) => strip_decl(&mt.pat),
		_ => pattern,
	}
}

fn is_decl(pattern: &Sourced<Pattern>) -> bool {
	match pattern.deref() {
		Pattern::Let(_) => true,
		Pattern::Mut(mt) => is_decl(&mt.pat),
		_ => false,
	}
}

fn bin_op_code(op: BinOpKind, lhs: &str, rhs: &str) -> String {
	let op = match op {
		BinOpKind::Arith(op) => match op.kind {
			ArithBinOpKind::Add => "+",
			ArithBinOpKind::Sub => "-",
			ArithBinOpKind::Mul => "*",
			// lua's `//` rounds down, but division rounds toward zero
			ArithBinOpKind::Div => return format!("__div({}, {})", lhs, rhs),
			ArithBinOpKind::Mod => return format!("__mod({}, {})", lhs, rhs),
		},
		BinOpKind::Bitwise(BitwiseBinOpKind::Shl) => "<<",
		BinOpKind::Bitwise(BitwiseBinOpKind::Shr) => ">>",
		BinOpKind::Cmp(op) => match op {
			CmpBinOpKind::Eq => "==",
			CmpBinOpKind::Ne => "~=",
			CmpBinOpKind::Lt => "<",
			CmpBinOpKind::Le => "<=",
			CmpBinOpKind::Gt => ">",
			CmpBinOpKind::Ge => ">=",
		},
		BinOpKind::Logic(LogicBinOpKind::And) => "and",
		BinOpKind::Logic(LogicBinOpKind::Or) => "or",
	};
	format!("({} {} {})", lhs, op, rhs)
}

fn cast_code(expr: String, ty: &Sourced<TypeExpr>) -> String {
	let TypeExpr::Named(name) = ty.deref() else {
		return expr; //TODO: casts between tuples
	};
	match name.as_str() {
		"usize" => return format!("__int({}, 64, false)", expr),
		"isize" => return format!("__int({}, 64, true)", expr),
		"str" => return expr,
		_ => { },
	}
	let (kind, bits) = name.split_at(1);
	match kind {
		"i" => format!("__int({}, {}, true)", expr, bits),
		"u" => format!("__int({}, {}, false)", expr, bits),
		"f" => format!("__float({})", expr),
		"b" if bits == "1" => format!("__bool({})", expr),
		"b" => format!("__bits({}, {})", expr, bits),
		_ => expr,
	}
}
