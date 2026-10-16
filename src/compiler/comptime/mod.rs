//! Compile time evaluation.
//!
//! Runs as much of the program as possible at compile time, and gives back the residual program: the part that still has to run.
//! Comptime-only types keep infinite precision. Sized types are evaluated exactly like runtime would, so the two never diverge.
//! Anything that needs runtime, like an extern call or a value read with `scanf`, stays in the residual program,
//! along with the variables it touches. Loops and calls run at compile time when all of them can, and are left for runtime otherwise.

mod error;
mod eval;
mod rational;
mod value;
#[cfg(test)]
mod tests;

use std::{cell::RefCell, collections::{HashMap, HashSet}, rc::Rc};

use num_bigint::BigInt;
use num_traits::{Signed, Zero};

pub use error::{EvalError, EvalErrorKind};
pub use value::Value;

use crate::compiler::{
	lexer::literal::{IntLit, IntType},
	parser::{node_sets::*, nodes::*, Var},
	semantic_analyzer::Type,
	source::{self, Sourced},
};

use rational::Rational;

/// How many loop iterations and calls a loop or call may take at compile time before it's left for runtime instead.
const STEP_LIMIT: u64 = 1_000_000;

pub struct Evaluated {
	/// What's left to run at runtime.
	pub ast: Sourced<Expr>,
	/// The file's last statement, when it's known at compile time.
	pub last_value: Option<String>,
}

pub fn evaluate(ast: &Sourced<Expr>) -> Result<Evaluated, Vec<EvalError>> {
	let mut evaluator = Evaluator {
		slots: Vec::new(),
		vars: HashMap::new(),
		buffers: Vec::new(),
		next_buffer_id: 0,
		next_temp_id: 0,
		loops: Vec::new(),
		funcs: Vec::new(),
		regions: Vec::new(),
		sandbox: 0,
		steps: 0,
		errors: Vec::new(),
		extern_types: HashMap::new(),
		budgets: Vec::new(),
	};

	let range = *ast.source();
	let result = match &**ast {
		Expr::Block(block) => evaluator.file(block, range),
		_ => evaluator.block_of(ast, range).map(|ast| (ast, None)),
	};
	match result {
		Ok((ast, last_value)) if evaluator.errors.is_empty() => Ok(Evaluated { ast, last_value }),
		_ => Err(evaluator.errors),
	}
}

/// Why evaluation stopped early.
enum Signal<'a> {
	/// A `break` out of a loop running at compile time.
	Break(Option<Value<'a>>),
	Continue,
	/// A `ret` out of a call running at compile time.
	Return(Value<'a>),
	/// The residual code never falls through, like after a runtime `ret`. Holds residual code that still has to be placed.
	Diverged(Option<Sourced<Expr>>),
	/// Something needs runtime inside code that's trying to run fully at compile time.
	Abort,
	/// An error was reported.
	Error,
}

type R<'a, T> = Result<T, Signal<'a>>;

/// The result of evaluating an expression: a value, or the residual code that computes it at runtime.
enum PVal<'a> {
	Static(Value<'a>),
	Dynamic(Sourced<Expr>),
}

#[derive(Debug, Clone)]
enum State<'a> {
	Static(Value<'a>),
	Dynamic,
}

#[derive(Debug, Clone)]
struct Slot<'a> {
	var: Rc<RefCell<Var>>,
	state: State<'a>,
	mutable: bool,
	/// Where the residual declaration goes, filled in once the variable is needed at runtime.
	placeholder: Option<(usize, usize)>,
	declared: bool,
}

#[derive(Debug, Clone)]
struct Buffer {
	id: usize,
	/// `None` is a placeholder for a declaration that may never be needed.
	stmts: Vec<Option<Sourced<Expr>>>,
}

struct LoopFrame {
	is_static: bool,
	region_depth: usize,
}

struct FuncFrame {
	is_static: bool,
	region_depth: usize,
}

/// Residual code like a runtime loop body, which may run any number of times.
/// Variables from outside it that it changes have to live at runtime from before it.
struct Region {
	first_slot: usize,
	written: HashSet<usize>,
}

#[derive(Clone)]
struct Snapshot<'a> {
	slots: Vec<Slot<'a>>,
	vars: HashMap<*const RefCell<Var>, usize>,
	buffers: Vec<Buffer>,
}

struct Evaluator<'a> {
	slots: Vec<Slot<'a>>,
	vars: HashMap<*const RefCell<Var>, usize>,
	buffers: Vec<Buffer>,
	next_buffer_id: usize,
	next_temp_id: usize,
	loops: Vec<LoopFrame>,
	funcs: Vec<FuncFrame>,
	regions: Vec<Region>,
	/// How many nested pieces of code are trying to run fully at compile time.
	sandbox: usize,
	steps: u64,
	errors: Vec<EvalError>,
	extern_types: HashMap<*const RefCell<Var>, Sourced<TypeExpr>>,
	/// The step count when each loop or call trying to run at compile time started.
	budgets: Vec<u64>,
}

fn node(value: Expr, range: source::Range) -> Sourced<Expr> {
	Sourced::new(value, range)
}

fn stmt(value: Expr, range: source::Range) -> Sourced<Expr> {
	node(Expr::Stmt(Stmt { expr: Box::new(node(value, range)) }), range)
}

fn identifier(var: &Rc<RefCell<Var>>, range: source::Range) -> Sourced<Expr> {
	node(Expr::Identifier(var.clone()), range)
}

fn declaration(var: &Rc<RefCell<Var>>, mutable: bool, rhs: Sourced<Expr>, range: source::Range) -> Sourced<Expr> {
	let binding = Sourced::new(Pattern::Binding(var.clone()), range);
	let binding = if mutable { Sourced::new(Pattern::Mut(Mut { pat: Box::new(binding) }), range) } else { binding };
	let pattern = Sourced::new(Pattern::Let(Let { pat: Box::new(binding) }), range);
	stmt(Expr::Assign(Assign { op: None, lhs: Box::new(pattern), rhs: Box::new(rhs) }), range)
}

fn int_type(ty: &Type) -> Option<IntType> {
	Some(match ty {
		Type::Int { signed: true, bits } => IntType::Signed(*bits),
		Type::Int { signed: false, bits } => IntType::Unsigned(*bits),
		Type::Usize => IntType::Unsigned(64),
		Type::Isize => IntType::Signed(64),
		Type::Bool(bits) => IntType::Bool(*bits),
		_ => return None,
	})
}

impl<'a> Evaluator<'a> {
	fn error<T>(&mut self, kind: EvalErrorKind, range: source::Range) -> R<'a, T> {
		self.errors.push(EvalError { kind, range });
		Err(Signal::Error)
	}

	fn check<T>(&mut self, result: Result<T, EvalErrorKind>, range: source::Range) -> R<'a, T> {
		match result {
			Ok(value) => Ok(value),
			Err(kind) => self.error(kind, range),
		}
	}

	/// Counts a loop iteration or a call. Code that takes too long, like an infinite loop, is left for runtime.
	fn step(&mut self) -> R<'a, ()> {
		self.steps += 1;
		match self.budgets.last() {
			Some(start) if self.steps - start > STEP_LIMIT => Err(Signal::Abort),
			_ => Ok(()),
		}
	}

	fn snapshot(&self) -> Snapshot<'a> {
		Snapshot { slots: self.slots.clone(), vars: self.vars.clone(), buffers: self.buffers.clone() }
	}

	fn restore(&mut self, snapshot: Snapshot<'a>) {
		self.slots = snapshot.slots;
		self.vars = snapshot.vars;
		self.buffers = snapshot.buffers;
	}

	// === residual code ===

	fn emit(&mut self, value: Sourced<Expr>) -> R<'a, ()> {
		if self.sandbox > 0 {
			return Err(Signal::Abort);
		}
		self.buffers.last_mut().expect("there's always a buffer while evaluating").stmts.push(Some(value));
		Ok(())
	}

	/// Residual code for a value that's needed at runtime. Code inside a sandbox can't need runtime.
	fn dynamic(&mut self, value: Sourced<Expr>) -> R<'a, PVal<'a>> {
		if self.sandbox > 0 {
			return Err(Signal::Abort);
		}
		Ok(PVal::Dynamic(value))
	}

	/// Runs `f` with the statements it emits collected into a new block.
	fn buffered<T>(&mut self, f: impl FnOnce(&mut Self) -> R<'a, T>) -> (Vec<Sourced<Expr>>, R<'a, T>) {
		self.next_buffer_id += 1;
		self.buffers.push(Buffer { id: self.next_buffer_id, stmts: Vec::new() });
		let result = f(self);
		let buffer = self.buffers.pop().expect("buffers are pushed and popped in pairs");
		(buffer.stmts.into_iter().flatten().collect(), result)
	}

	/// Evaluates `node` into residual code, as a block when it needs statements.
	fn captured(&mut self, node: &'a Sourced<Expr>) -> R<'a, Sourced<Expr>> {
		let range = *node.source();
		let (stmts, result) = self.buffered(|this| this.eval(node));
		let expr = match result {
			Ok(PVal::Static(Value::Nothing)) => None,
			Ok(PVal::Static(value)) => Some(self.residual(value, range)?),
			Ok(PVal::Dynamic(expr)) => Some(expr),
			Err(Signal::Diverged(rest)) => {
				let mut stmts = stmts;
				stmts.extend(rest.map(|rest| stmt(rest.inner(), range)));
				return Ok(node_block(stmts, None, range));
			},
			Err(signal) => return Err(signal),
		};
		Ok(match (stmts.is_empty(), expr) {
			(true, Some(expr)) => expr,
			(_, expr) => node_block(stmts, expr, range),
		})
	}

	fn block_of(&mut self, node: &'a Sourced<Expr>, range: source::Range) -> R<'a, Sourced<Expr>> {
		let expr = self.captured(node)?;
		Ok(match &*expr {
			Expr::Block(_) => expr,
			_ => node_block(vec![stmt(expr.inner(), range)], None, range),
		})
	}

	/// Code for a comptime value at runtime.
	fn residual(&mut self, value: Value<'a>, range: source::Range) -> R<'a, Sourced<Expr>> {
		if self.sandbox > 0 {
			return Err(Signal::Abort);
		}
		let int_lit = |magnitude: BigInt, ty: Option<IntType>| {
			let lit = node(Expr::IntLit(IntLit { value: magnitude.magnitude().clone(), ty }), range);
			if magnitude.is_negative() {
				node(Expr::UnaOp(UnaOp { op: UnaOpKind::Negate, expr: Box::new(lit) }), range)
			} else {
				lit
			}
		};
		Ok(match value {
			Value::Int { value, ty } if ty.is_comptime() => {
				// runtime defaults to i64, so the value has to fit
				if value.bits() > 63 {
					return self.error(EvalErrorKind::TooLargeForRuntime { value: value.to_string() }, range);
				}
				int_lit(value, None)
			},
			Value::Int { value, ty } => int_lit(value, int_type(&ty)),
			Value::Bool { value, ty: Type::Bool(1) } => node(Expr::BoolLit(!value.is_zero()), range),
			// a single unsized bit is a b1 at runtime, more need a size
			Value::Bool { value, ty: Type::ComptimeBool } if value.bits() <= 1 => node(Expr::BoolLit(!value.is_zero()), range),
			Value::Bool { value, ty: Type::ComptimeBool } => return self.error(EvalErrorKind::UnsizedAtRuntime { value: format!("0x{:x}", value) }, range),
			Value::Bool { value, ty } => node(Expr::IntLit(IntLit { value, ty: int_type(&ty) }), range),
			Value::Real(value) => self.real_lit(value.to_f64(), None, range)?,
			Value::Float { value, bits } => self.real_lit(value, Some(bits), range)?,
			Value::Str(value) => node(Expr::StrLit(value), range),
			Value::Tuple(items) => {
				let mut residual_items = Vec::new();
				for item in items {
					residual_items.push(self.residual(item, range)?);
				}
				node(Expr::TupleCtor(TupleCtor { items: residual_items }), range)
			},
			Value::Nothing => node_block(Vec::new(), None, range),
			Value::Func(func) => match &func.name {
				Some(name) => {
					let name = name.clone();
					self.declare_runtime_var(&name, range)?;
					identifier(&name, range)
				},
				None => self.residual_func(func.node, range)?,
			},
			Value::Extern(var) => {
				self.declare_runtime_var(&var, range)?;
				identifier(&var, range)
			},
			Value::Ptr(slot) => {
				// the variable's address reaches runtime, so from here on it lives at runtime
				self.dynamize(slot, range)?;
				let var = self.slots[slot].var.clone();
				node(Expr::UnaOp(UnaOp { op: UnaOpKind::AddressOf, expr: Box::new(identifier(&var, range)) }), range)
			},
		})
	}

	fn real_lit(&mut self, value: f64, num_bits: Option<u64>, range: source::Range) -> R<'a, Sourced<Expr>> {
		let Some((negative, lit)) = Rational::from_f64(value).and_then(|value| value.to_lit(num_bits)) else {
			return self.error(EvalErrorKind::TooLargeForRuntime { value: value.to_string() }, range);
		};
		let lit = node(Expr::RealLit(lit), range);
		Ok(if negative { node(Expr::UnaOp(UnaOp { op: UnaOpKind::Negate, expr: Box::new(lit) }), range) } else { lit })
	}

	fn to_expr(&mut self, value: PVal<'a>, range: source::Range) -> R<'a, Sourced<Expr>> {
		match value {
			PVal::Static(value) => self.residual(value, range),
			PVal::Dynamic(expr) => Ok(expr),
		}
	}

	// === variables ===

	fn slot_of(&self, var: &Rc<RefCell<Var>>) -> Option<usize> {
		self.vars.get(&Rc::as_ptr(var)).copied()
	}

	/// Declares a variable. Its residual declaration is only written if runtime ever needs it.
	fn declare(&mut self, var: &Rc<RefCell<Var>>, state: State<'a>, mutable: bool) -> usize {
		let buffer = self.buffers.last_mut().expect("there's always a buffer while evaluating");
		let placeholder = Some((buffer.id, buffer.stmts.len()));
		buffer.stmts.push(None);
		self.slots.push(Slot { var: var.clone(), state, mutable, placeholder, declared: false });
		let slot = self.slots.len() - 1;
		self.vars.insert(Rc::as_ptr(var), slot);
		slot
	}

	/// Writes the residual declaration of a variable that's static so far, with its current value.
	fn declare_runtime_var(&mut self, var: &Rc<RefCell<Var>>, range: source::Range) -> R<'a, ()> {
		let Some(slot) = self.slot_of(var) else { return Ok(()) };
		if self.slots[slot].declared {
			return Ok(());
		}
		if self.sandbox > 0 {
			return Err(Signal::Abort);
		}
		let State::Static(value) = self.slots[slot].state.clone() else {
			self.slots[slot].declared = true;
			return Ok(());
		};
		let name = var.borrow().name.clone();
		let rhs = match &value {
			_ if value.is_comptime_only() => return self.error(EvalErrorKind::NeedsRuntimeType { name, ty: value.ty() }, range),
			Value::Func(func) => self.residual_func(func.node, range)?,
			// an extern's declaration is the extern itself
			Value::Extern(var) => {
				let decl = self.extern_decl(var, range);
				self.slots[slot].declared = true;
				return self.fill_placeholder(slot, decl);
			},
			_ => self.residual(value.clone(), range)?,
		};
		let decl = declaration(var, self.slots[slot].mutable, rhs, range);
		self.slots[slot].declared = true;
		self.fill_placeholder(slot, decl)
	}

	fn fill_placeholder(&mut self, slot: usize, decl: Sourced<Expr>) -> R<'a, ()> {
		let Some((buffer_id, index)) = self.slots[slot].placeholder else {
			return self.emit(decl);
		};
		match self.buffers.iter_mut().find(|buffer| buffer.id == buffer_id) {
			Some(buffer) => buffer.stmts[index] = Some(decl),
			// the declaration's block is already done, so this is the variable's first use in this one
			None => self.emit(decl)?,
		}
		Ok(())
	}

	/// Moves a variable to runtime for good, like when its address escapes or a runtime loop changes it.
	fn dynamize(&mut self, slot: usize, range: source::Range) -> R<'a, ()> {
		if matches!(self.slots[slot].state, State::Dynamic) {
			return Ok(());
		}
		if self.sandbox > 0 {
			return Err(Signal::Abort);
		}
		let var = self.slots[slot].var.clone();
		self.declare_runtime_var(&var, range)?;
		self.slots[slot].state = State::Dynamic;
		Ok(())
	}

	fn write(&mut self, slot: usize, state: State<'a>) {
		if let Some(region) = self.regions.last_mut()
			&& slot < region.first_slot
			&& matches!(self.slots[slot].state, State::Static(_))
		{
			region.written.insert(slot);
		}
		self.slots[slot].state = state;
	}

	fn temp(&mut self, value: Value<'a>) -> usize {
		self.next_temp_id += 1;
		let var = Rc::new(RefCell::new(Var::new(format!("__comptime{}", self.next_temp_id))));
		self.declare(&var, State::Static(value), true)
	}

	fn extern_decl(&self, var: &Rc<RefCell<Var>>, range: source::Range) -> Sourced<Expr> {
		let ty = self.extern_types.get(&Rc::as_ptr(var)).cloned().expect("externs are recorded when declared");
		stmt(Expr::Extern(Extern { binding: var.clone(), ty }), range)
	}

	// === regions ===

	/// Evaluates residual code that may run any number of times, like a runtime loop body.
	/// Variables from outside that it changes move to runtime before it, and it's evaluated again.
	fn region<T>(&mut self, range: source::Range, mut f: impl FnMut(&mut Self) -> R<'a, T>) -> R<'a, T> {
		loop {
			let snapshot = self.snapshot();
			self.regions.push(Region { first_slot: self.slots.len(), written: HashSet::new() });
			let result = f(self);
			let region = self.regions.pop().expect("regions are pushed and popped in pairs");
			let written = region.written.into_iter().filter(|&slot| matches!(snapshot.slots.get(slot).map(|slot| &slot.state), Some(State::Static(_)))).collect::<Vec<_>>();
			if written.is_empty() {
				return result;
			}
			self.restore(snapshot);
			for slot in written {
				self.dynamize(slot, range)?;
			}
		}
	}
}

fn node_block(stmts: Vec<Sourced<Expr>>, expr: Option<Sourced<Expr>>, range: source::Range) -> Sourced<Expr> {
	node(Expr::Block(Block { stmts, expr: expr.map(Box::new) }), range)
}
