use std::{mem, rc::Rc};

use num_bigint::{BigInt, BigUint};

use crate::compiler::{
	lexer::literal::IntType,
	parser::{node_sets::*, nodes::*},
	semantic_analyzer::Type,
	source::{self, Sourced},
};

use super::{
	node, node_block, stmt, identifier, declaration,
	rational::Rational,
	value::{self, FuncValue, Value},
	Evaluator, FuncFrame, LoopFrame, PVal, Signal, State, R,
};

impl<'a> Evaluator<'a> {
	/// The whole program. Its last statement's value is kept for interactive mode.
	pub(super) fn file(&mut self, block: &'a Block, range: source::Range) -> R<'a, (Sourced<Expr>, Option<String>)> {
		let mut last_value = None;
		let (stmts, result) = self.buffered(|this| {
			for (i, item) in block.stmts.iter().enumerate() {
				let is_last = i + 1 == block.stmts.len();
				match this.eval(item) {
					Ok(PVal::Static(value)) => if is_last && !matches!(item.inner_ref(), Expr::Stmt(_)) {
						last_value = Some(value.to_string());
					},
					Ok(PVal::Dynamic(expr)) => this.emit(stmt(expr.inner(), *item.source()))?,
					Err(Signal::Diverged(rest)) => {
						if let Some(rest) = rest {
							this.emit(stmt(rest.inner(), *item.source()))?;
						}
						break;
					},
					Err(signal) => return Err(signal),
				}
			}
			Ok(())
		});
		result?;
		Ok((node_block(stmts, None, range), last_value))
	}

	pub(super) fn eval(&mut self, node_ref: &'a Sourced<Expr>) -> R<'a, PVal<'a>> {
		let range = *node_ref.source();
		match &**node_ref {
			Expr::Assign(assign) => {
				self.assign(assign, range)?;
				Ok(PVal::Static(Value::void()))
			},
			Expr::Return(ret) => {
				let value = match &ret.expr {
					Some(expr) => self.eval(expr)?,
					None => PVal::Static(Value::void()),
				};
				match self.funcs.last() {
					Some(FuncFrame { is_static: true, region_depth }) if *region_depth == self.regions.len() => match value {
						PVal::Static(value) => Err(Signal::Return(value)),
						PVal::Dynamic(_) => Err(Signal::Abort),
					},
					Some(FuncFrame { is_static: true, .. }) => Err(Signal::Abort),
					// runtime code, or the program's exit code
					_ => {
						let expr = match ret.expr {
							Some(_) => Some(Box::new(self.to_expr(value, range)?)),
							None => None,
						};
						self.emit(stmt(Expr::Return(Return { expr }), range))?;
						Err(Signal::Diverged(None))
					},
				}
			},
			Expr::Break(brk) => {
				let value = match &brk.expr {
					Some(expr) => Some(self.eval(expr)?),
					None => None,
				};
				match self.loops.last() {
					Some(LoopFrame { is_static: true, region_depth }) if *region_depth == self.regions.len() => match value {
						None => Err(Signal::Break(None)),
						Some(PVal::Static(value)) => Err(Signal::Break(Some(value))),
						Some(PVal::Dynamic(_)) => Err(Signal::Abort),
					},
					Some(LoopFrame { is_static: true, .. }) | None => Err(Signal::Abort),
					Some(LoopFrame { is_static: false, .. }) => {
						let expr = match value {
							Some(value) => Some(Box::new(self.to_expr(value, range)?)),
							None => None,
						};
						self.emit(stmt(Expr::Break(Break { expr }), range))?;
						Err(Signal::Diverged(None))
					},
				}
			},
			Expr::Continue => match self.loops.last() {
				Some(LoopFrame { is_static: true, region_depth }) if *region_depth == self.regions.len() => Err(Signal::Continue),
				Some(LoopFrame { is_static: true, .. }) | None => Err(Signal::Abort),
				Some(LoopFrame { is_static: false, .. }) => {
					self.emit(stmt(Expr::Continue, range))?;
					Err(Signal::Diverged(None))
				},
			},
			// earlier runtime code may never finish, so reaching this at compile time doesn't mean runtime will. it stays a runtime trap
			Expr::Unreachable => {
				self.emit(stmt(Expr::Unreachable, range))?;
				Err(Signal::Diverged(None))
			},

			Expr::Plex(_) => self.dynamic(node_ref.clone()), //TODO: plex

			Expr::Loop(lp) => self.lp(None, &lp.body, None, range, |_, body, _| Expr::Loop(Loop { body: Box::new(body) })),
			Expr::While(wh) => self.lp(Some(&wh.cond), &wh.body, wh.body_else.as_deref(), range, |cond, body, body_else| Expr::While(While {
				cond: Box::new(cond.expect("a while has a condition")),
				body: Box::new(body),
				body_else: body_else.map(Box::new),
			})),
			Expr::For(fr) => self.for_loop(fr, range),
			Expr::If(node_if) => self.if_else(node_if, range),

			Expr::Block(block) => self.block(block, range),
			Expr::Stmt(inner) => {
				match self.eval(&inner.expr)? {
					PVal::Static(_) => { },
					PVal::Dynamic(expr) => self.emit(stmt(expr.inner(), range))?,
				}
				Ok(PVal::Static(Value::Nothing))
			},
			Expr::BinOp(bin_op) => self.bin_op(bin_op, range),
			Expr::UnaOp(una_op) => self.una_op(una_op, range),
			Expr::FieldAccess(field_access) => {
				let expr = self.eval(&field_access.expr)?;
				let expr = self.to_expr(expr, range)?;
				self.dynamic(node(Expr::FieldAccess(FieldAccess { expr: Box::new(expr), ident: field_access.ident.clone() }), range))
			},
			Expr::Func(func) => Ok(PVal::Static(Value::Func(FuncValue { node: func, name: None }))),
			Expr::Call(call) => self.call(call, range),
			Expr::Cast(cast) => {
				let value = self.eval(&cast.expr)?;
				let to = Type::from_type_expr(&cast.ty).unwrap_or(Type::Unknown);
				if let PVal::Static(value) = &value
					&& let Some(result) = self.check(value::cast(value, &to), range)?
				{
					return Ok(PVal::Static(result));
				}
				let expr = self.to_expr(value, range)?;
				self.dynamic(node(Expr::Cast(Cast { expr: Box::new(expr), ty: cast.ty.clone() }), range))
			},
			Expr::Extern(ext) => {
				self.extern_types.insert(Rc::as_ptr(&ext.binding), ext.ty.clone());
				self.declare(&ext.binding, State::Static(Value::Extern(ext.binding.clone())), false);
				Ok(PVal::Static(Value::void()))
			},

			Expr::TupleCtor(tuple) => {
				let items = self.eval_all(&tuple.items)?;
				if items.iter().all(|item| matches!(item, PVal::Static(_))) {
					return Ok(PVal::Static(Value::Tuple(items.into_iter().map(|item| match item {
						PVal::Static(value) => value,
						PVal::Dynamic(_) => unreachable!("checked above"),
					}).collect())));
				}
				let mut exprs = Vec::new();
				for item in items {
					exprs.push(self.to_expr(item, range)?);
				}
				self.dynamic(node(Expr::TupleCtor(TupleCtor { items: exprs }), range))
			},
			Expr::BoolLit(value) => Ok(PVal::Static(Value::Bool { value: BigUint::from(*value as u8), ty: Type::ComptimeBool })),
			Expr::IntLit(lit) => Ok(PVal::Static(match lit.ty {
				None => Value::Int { value: BigInt::from(lit.value.clone()), ty: Type::ComptimeInt },
				Some(IntType::Signed(bits)) => Value::Int { value: BigInt::from(lit.value.clone()), ty: Type::Int { signed: true, bits } },
				Some(IntType::Unsigned(bits)) => Value::Int { value: BigInt::from(lit.value.clone()), ty: Type::Int { signed: false, bits } },
				Some(IntType::Bool(bits)) => Value::Bool { value: lit.value.clone(), ty: Type::Bool(bits) },
				Some(IntType::Bits) => Value::Bool { value: lit.value.clone(), ty: Type::ComptimeBool },
			})),
			Expr::RealLit(lit) => match lit.num_bits {
				None => Ok(PVal::Static(Value::Real(Rational::from_lit(lit)))),
				Some(bits @ (32 | 64)) => {
					let value = Rational::from_lit(lit).to_f64();
					let value = if bits == 32 { value as f32 as f64 } else { value };
					Ok(PVal::Static(Value::Float { value, bits }))
				},
				// other float sizes can't be emulated, so they stay at runtime
				Some(_) => self.dynamic(node_ref.clone()),
			},
			Expr::StrLit(value) => Ok(PVal::Static(Value::Str(value.clone()))),
			Expr::Identifier(var) => match self.slot_of(var) {
				Some(slot) => match &self.slots[slot].state {
					State::Static(value) => Ok(PVal::Static(value.clone())),
					State::Dynamic => self.dynamic(identifier(var, range)),
				},
				None => self.dynamic(identifier(var, range)),
			},
		}
	}

	/// Evaluates expressions left to right. When a later one diverges, the earlier ones' runtime effects are kept.
	fn eval_all(&mut self, nodes: &'a [Sourced<Expr>]) -> R<'a, Vec<PVal<'a>>> {
		let mut values = Vec::new();
		for item in nodes {
			match self.eval(item) {
				Ok(value) => values.push(value),
				Err(Signal::Diverged(rest)) => {
					for value in values {
						if let PVal::Dynamic(expr) = value {
							let range = *expr.source();
							self.emit(stmt(expr.inner(), range))?;
						}
					}
					return Err(Signal::Diverged(rest));
				},
				Err(signal) => return Err(signal),
			}
		}
		Ok(values)
	}

	fn block(&mut self, block: &'a Block, range: source::Range) -> R<'a, PVal<'a>> {
		let (stmts, result) = self.buffered(|this| {
			for item in block.stmts.iter() {
				match this.eval(item)? {
					PVal::Static(_) => { },
					PVal::Dynamic(expr) => this.emit(stmt(expr.inner(), *item.source()))?,
				}
			}
			match &block.expr {
				Some(expr) => this.eval(expr),
				None => Ok(PVal::Static(Value::Nothing)),
			}
		});
		match result {
			Ok(PVal::Static(value)) if stmts.is_empty() => Ok(PVal::Static(value)),
			Ok(value) => {
				let expr = match value {
					PVal::Static(Value::Nothing) => None,
					value => Some(self.to_expr(value, range)?),
				};
				self.dynamic(node_block(stmts, expr, range))
			},
			// the block's code up to the diverging statement still has to run
			Err(Signal::Diverged(rest)) => {
				let mut stmts = stmts;
				stmts.extend(rest.map(|rest| stmt(rest.inner(), range)));
				Err(Signal::Diverged(Some(node_block(stmts, None, range))))
			},
			Err(signal) => Err(signal),
		}
	}

	fn if_else(&mut self, node_if: &'a If, range: source::Range) -> R<'a, PVal<'a>> {
		match self.eval(&node_if.cond)? {
			PVal::Static(cond) => {
				let cond = cond.as_bool().expect("the analyzer checks that conditions are bools");
				match (cond, &node_if.body_else) {
					(true, _) => self.scoped(|this| this.eval(&node_if.body)),
					(false, Some(body_else)) => self.scoped(|this| this.eval(body_else)),
					(false, None) => Ok(PVal::Static(Value::void())),
				}
			},
			PVal::Dynamic(cond) => {
				if self.sandbox > 0 {
					return Err(Signal::Abort);
				}
				// either branch may run, so the variables they change have to live at runtime
				let (body, body_else) = self.region(range, |this| {
					let body = this.scoped(|this| this.captured(&node_if.body))?;
					let body_else = match &node_if.body_else {
						Some(body_else) => Some(this.scoped(|this| this.captured(body_else))?),
						None => None,
					};
					Ok((body, body_else))
				})?;
				self.dynamic(node(Expr::If(If { cond: Box::new(cond), body: Box::new(body), body_else: body_else.map(Box::new) }), range))
			},
		}
	}

	/// Forgets the variables `f` declares afterwards, like a block does.
	fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> R<'a, T>) -> R<'a, T> {
		let vars = self.vars.clone();
		let result = f(self);
		self.vars = vars;
		result
	}

	/// A `loop` or `while`. It runs at compile time when every iteration can, and is left for runtime otherwise.
	fn lp(
		&mut self,
		cond: Option<&'a Sourced<Expr>>,
		body: &'a Sourced<Expr>,
		body_else: Option<&'a Sourced<Expr>>,
		range: source::Range,
		make: impl Fn(Option<Sourced<Expr>>, Sourced<Expr>, Option<Sourced<Expr>>) -> Expr,
	) -> R<'a, PVal<'a>> {
		let snapshot = self.snapshot();
		self.sandbox += 1;
		self.budgets.push(self.steps);
		self.loops.push(LoopFrame { is_static: true, region_depth: self.regions.len() });
		let result = self.static_loop(cond, body);
		self.loops.pop();
		self.budgets.pop();
		self.sandbox -= 1;

		match result {
			// ran to the end, so a `while` runs its `else`
			Ok(None) => match body_else {
				Some(body_else) => self.scoped(|this| this.eval(body_else)).map(|_| PVal::Static(Value::void())),
				None => Ok(PVal::Static(Value::void())),
			},
			Ok(Some(value)) => Ok(PVal::Static(value.unwrap_or_else(Value::void))),
			Err(Signal::Abort) if self.sandbox == 0 => {
				self.restore(snapshot);
				let (cond, body, body_else) = self.region(range, |this| {
					this.loops.push(LoopFrame { is_static: false, region_depth: this.regions.len() });
					let result = (|| {
						let cond = match cond {
							Some(cond) => Some(this.captured(cond)?),
							None => None,
						};
						let body = this.scoped(|this| this.captured(body))?;
						Ok((cond, body))
					})();
					this.loops.pop();
					let (cond, body) = result?;
					let body_else = match body_else {
						Some(body_else) => Some(this.scoped(|this| this.captured(body_else))?),
						None => None,
					};
					Ok((cond, body, body_else))
				})?;
				self.dynamic(node(make(cond, body, body_else), range))
			},
			Err(signal) => Err(signal),
		}
	}

	/// Runs a loop at compile time. `Ok(None)` when the condition ended it, `Ok(Some(..))` when it broke out.
	fn static_loop(&mut self, cond: Option<&'a Sourced<Expr>>, body: &'a Sourced<Expr>) -> R<'a, Option<Option<Value<'a>>>> {
		loop {
			self.step()?;
			if let Some(cond) = cond {
				match self.eval(cond)? {
					PVal::Static(value) => if value.as_bool() != Some(true) {
						return Ok(None);
					},
					PVal::Dynamic(_) => return Err(Signal::Abort),
				}
			}
			match self.scoped(|this| this.eval(body)) {
				Ok(_) | Err(Signal::Continue) => { },
				Err(Signal::Break(value)) => return Ok(Some(value)),
				Err(signal) => return Err(signal),
			}
		}
	}

	/// `for` loops need iterators, which only exist at runtime for now.
	fn for_loop(&mut self, fr: &'a For, range: source::Range) -> R<'a, PVal<'a>> {
		let iter = self.eval(&fr.iter)?;
		let iter = self.to_expr(iter, range)?;
		if self.sandbox > 0 {
			return Err(Signal::Abort);
		}
		let (body, body_else) = self.region(range, |this| {
			this.loops.push(LoopFrame { is_static: false, region_depth: this.regions.len() });
			let body = this.scoped(|this| {
				this.declare_runtime_pattern(&fr.binding);
				this.captured(&fr.body)
			});
			this.loops.pop();
			let body = body?;
			let body_else = match &fr.body_else {
				Some(body_else) => Some(this.scoped(|this| this.captured(body_else))?),
				None => None,
			};
			Ok((body, body_else))
		})?;
		self.dynamic(node(Expr::For(For { binding: fr.binding.clone(), iter: Box::new(iter), body: Box::new(body), body_else: body_else.map(Box::new) }), range))
	}

	/// Declares the bindings of a pattern whose value only exists at runtime, like a function's arguments.
	fn declare_runtime_pattern(&mut self, pattern: &'a Sourced<Pattern>) {
		match &**pattern {
			Pattern::Let(lt) => self.declare_runtime_pattern(&lt.pat),
			Pattern::Mut(mt) => self.declare_runtime_pattern(&mt.pat),
			Pattern::TupleDtor(dtor) => for item in dtor.items.iter() {
				self.declare_runtime_pattern(item);
			},
			Pattern::Binding(var) => {
				let slot = self.declare(var, State::Dynamic, true);
				self.slots[slot].declared = true;
			},
			Pattern::Deref(_) | Pattern::Discard => { },
		}
	}

	fn bin_op(&mut self, bin_op: &'a BinOp, range: source::Range) -> R<'a, PVal<'a>> {
		if let BinOpKind::Logic(logic) = bin_op.op {
			// b1 logic short-circuits, so the right side only runs when it's needed
			let lhs = self.eval(&bin_op.lhs)?;
			if let PVal::Static(lhs_value) = &lhs {
				// unsized bools with more than one bit don't short-circuit, they're bitwise
				let is_single_bit = !matches!(lhs_value, Value::Bool { value, ty: Type::ComptimeBool } if value.bits() > 1);
				let lhs_bool = lhs_value.as_bool().expect("the analyzer checks that logic is on bools");
				match (logic, lhs_bool) {
					(LogicBinOpKind::And, false) | (LogicBinOpKind::Or, true) if is_single_bit => return Ok(PVal::Static(lhs_value.clone())),
					_ => { },
				}
				return match self.eval(&bin_op.rhs)? {
					PVal::Static(rhs) => {
						let bitwise = match logic {
							LogicBinOpKind::And => BinOpKind::Bitwise(BitwiseBinOpKind::And),
							LogicBinOpKind::Or => BinOpKind::Bitwise(BitwiseBinOpKind::Or),
						};
						match self.check(value::bin_op(bitwise, lhs_value, &rhs), range)? {
							Some(value) => Ok(PVal::Static(value)),
							None => Ok(PVal::Static(Value::b1(rhs.as_bool() == Some(true)))),
						}
					},
					PVal::Dynamic(rhs) if is_single_bit => Ok(PVal::Dynamic(rhs)),
					PVal::Dynamic(rhs) => {
						let lhs = self.residual(lhs_value.clone(), range)?;
						self.dynamic(node(Expr::BinOp(BinOp { op: bin_op.op, lhs: Box::new(lhs), rhs: Box::new(rhs) }), range))
					},
				};
			}
			let lhs = self.to_expr(lhs, range)?;
			let rhs = self.region(range, |this| this.captured(&bin_op.rhs))?;
			return self.dynamic(node(Expr::BinOp(BinOp { op: bin_op.op, lhs: Box::new(lhs), rhs: Box::new(rhs) }), range));
		}

		let mut values = self.eval_two(&bin_op.lhs, &bin_op.rhs)?;
		let rhs = values.pop().expect("two values");
		let lhs = values.pop().expect("two values");
		if let (PVal::Static(a), PVal::Static(b)) = (&lhs, &rhs)
			&& let Some(value) = self.check(value::bin_op(bin_op.op, a, b), range)?
		{
			return Ok(PVal::Static(value));
		}
		let lhs = self.to_expr(lhs, range)?;
		let rhs = self.to_expr(rhs, range)?;
		self.dynamic(node(Expr::BinOp(BinOp { op: bin_op.op, lhs: Box::new(lhs), rhs: Box::new(rhs) }), range))
	}

	fn eval_two(&mut self, a: &'a Sourced<Expr>, b: &'a Sourced<Expr>) -> R<'a, Vec<PVal<'a>>> {
		let a = self.eval(a)?;
		match self.eval(b) {
			Ok(b) => Ok(vec![a, b]),
			Err(Signal::Diverged(rest)) => {
				if let PVal::Dynamic(expr) = a {
					let range = *expr.source();
					self.emit(stmt(expr.inner(), range))?;
				}
				Err(Signal::Diverged(rest))
			},
			Err(signal) => Err(signal),
		}
	}

	fn una_op(&mut self, una_op: &'a UnaOp, range: source::Range) -> R<'a, PVal<'a>> {
		if una_op.op == UnaOpKind::AddressOf {
			return match &**una_op.expr {
				Expr::Identifier(var) => match self.slot_of(var) {
					Some(slot) => Ok(PVal::Static(Value::Ptr(slot))),
					None => self.dynamic(node(Expr::UnaOp(UnaOp { op: una_op.op, expr: una_op.expr.clone() }), range)),
				},
				// `p.*.&` is `p`
				Expr::UnaOp(UnaOp { op: UnaOpKind::Deref, expr }) => self.eval(expr),
				// anything else points at a copy
				_ => match self.eval(&una_op.expr)? {
					PVal::Static(value) => Ok(PVal::Static(Value::Ptr(self.temp(value)))),
					PVal::Dynamic(expr) => self.dynamic(node(Expr::UnaOp(UnaOp { op: una_op.op, expr: Box::new(expr) }), range)),
				},
			};
		}

		let value = self.eval(&una_op.expr)?;
		if una_op.op == UnaOpKind::Deref {
			return match value {
				PVal::Static(Value::Ptr(slot)) => match &self.slots[slot].state {
					State::Static(value) => Ok(PVal::Static(value.clone())),
					State::Dynamic => {
						let var = self.slots[slot].var.clone();
						self.dynamic(identifier(&var, range))
					},
				},
				value => {
					let expr = self.to_expr(value, range)?;
					self.dynamic(node(Expr::UnaOp(UnaOp { op: una_op.op, expr: Box::new(expr) }), range))
				},
			};
		}

		if let PVal::Static(value) = &value
			&& let Some(result) = self.check(value::una_op(una_op.op, value), range)?
		{
			return Ok(PVal::Static(result));
		}
		let expr = self.to_expr(value, range)?;
		self.dynamic(node(Expr::UnaOp(UnaOp { op: una_op.op, expr: Box::new(expr) }), range))
	}

	fn call(&mut self, call: &'a Call, range: source::Range) -> R<'a, PVal<'a>> {
		let func = self.eval(&call.func)?;
		let Expr::TupleCtor(tuple) = &**call.arg else { unreachable!("call arguments are always tuples") };
		let args = self.eval_all(&tuple.items)?;

		// a call runs at compile time when everything it does can
		if let PVal::Static(Value::Func(func_value)) = &func
			&& args.iter().all(|arg| matches!(arg, PVal::Static(_)))
		{
			let args = Value::Tuple(args.iter().map(|arg| match arg {
				PVal::Static(value) => value.clone(),
				PVal::Dynamic(_) => unreachable!("checked above"),
			}).collect());
			let snapshot = self.snapshot();
			match self.static_call(func_value.node, args, range) {
				Ok(value) => return Ok(PVal::Static(value)),
				Err(Signal::Abort) if self.sandbox == 0 => self.restore(snapshot),
				Err(signal) => return Err(signal),
			}
		}

		let func = self.to_expr(func, range)?;
		let mut arg_exprs = Vec::new();
		for arg in args {
			arg_exprs.push(self.to_expr(arg, range)?);
		}
		let arg = node(Expr::TupleCtor(TupleCtor { items: arg_exprs }), *call.arg.source());
		self.dynamic(node(Expr::Call(Call { func: Box::new(func), arg: Box::new(arg) }), range))
	}

	fn static_call(&mut self, func: &'a Func, args: Value<'a>, range: source::Range) -> R<'a, Value<'a>> {
		self.sandbox += 1;
		self.budgets.push(self.steps);
		let loops = mem::take(&mut self.loops);
		let regions = mem::take(&mut self.regions);
		self.funcs.push(FuncFrame { is_static: true, region_depth: 0 });
		let result = self.scoped(|this| {
			this.step()?;
			this.bind(&func.binding, PVal::Static(args), true, false, range)?;
			this.eval(&func.body)
		});
		self.funcs.pop();
		self.regions = regions;
		self.loops = loops;
		self.budgets.pop();
		self.sandbox -= 1;
		match result {
			Ok(PVal::Static(value)) | Err(Signal::Return(value)) => Ok(value),
			Ok(PVal::Dynamic(_)) => Err(Signal::Abort),
			Err(signal) => Err(signal),
		}
	}

	/// A function with its body evaluated as far as it can be without knowing its arguments.
	pub(super) fn residual_func(&mut self, func: &'a Func, range: source::Range) -> R<'a, Sourced<Expr>> {
		let loops = mem::take(&mut self.loops);
		let regions = mem::take(&mut self.regions);
		let sandbox = mem::take(&mut self.sandbox);
		self.funcs.push(FuncFrame { is_static: false, region_depth: 0 });
		let body = self.scoped(|this| {
			this.declare_runtime_pattern(&func.binding);
			this.captured(&func.body)
		});
		self.funcs.pop();
		self.sandbox = sandbox;
		self.regions = regions;
		self.loops = loops;
		Ok(node(Expr::Func(Func { binding: func.binding.clone(), body: Box::new(body?) }), range))
	}

	fn assign(&mut self, assign: &'a Assign, range: source::Range) -> R<'a, ()> {
		let rhs = self.eval(&assign.rhs)?;
		let Some(op) = assign.op else {
			return self.bind(&assign.lhs, rhs, false, false, range);
		};

		// a compound assignment reads and writes the same place
		let place = match &**assign.lhs {
			Pattern::Binding(var) => self.slot_of(var).map(|slot| (slot, identifier(var, range))),
			Pattern::Deref(ptr) => match self.eval(ptr)? {
				PVal::Static(Value::Ptr(slot)) => {
					let var = self.slots[slot].var.clone();
					Some((slot, identifier(&var, range)))
				},
				PVal::Static(_) => None,
				PVal::Dynamic(ptr) => {
					let rhs = self.to_expr(rhs, range)?;
					let lhs = Sourced::new(Pattern::Deref(Box::new(ptr)), range);
					return self.emit(stmt(Expr::Assign(Assign { op: Some(op), lhs: Box::new(lhs), rhs: Box::new(rhs) }), range));
				},
			},
			_ => None,
		};
		let Some((slot, target)) = place else {
			let rhs = self.to_expr(rhs, range)?;
			return self.emit(stmt(Expr::Assign(Assign { op: Some(op), lhs: assign.lhs.clone(), rhs: Box::new(rhs) }), range));
		};

		if let (State::Static(current), PVal::Static(rhs)) = (self.slots[slot].state.clone(), &rhs)
			&& let Some(value) = self.check(value::bin_op(op, &current, rhs), range)?
		{
			self.write(slot, State::Static(value));
			return Ok(());
		}
		self.dynamize(slot, range)?;
		self.write(slot, State::Dynamic);
		let rhs = self.to_expr(rhs, range)?;
		let Expr::Identifier(var) = target.inner() else { unreachable!("targets are identifiers") };
		let lhs = Sourced::new(Pattern::Binding(var), range);
		self.emit(stmt(Expr::Assign(Assign { op: Some(op), lhs: Box::new(lhs), rhs: Box::new(rhs) }), range))
	}

	/// Stores a value into a pattern, declaring its bindings when `is_decl` is set.
	fn bind(&mut self, pattern: &'a Sourced<Pattern>, value: PVal<'a>, is_decl: bool, is_mut: bool, range: source::Range) -> R<'a, ()> {
		match &**pattern {
			Pattern::Let(lt) => self.bind(&lt.pat, value, true, is_mut, range),
			Pattern::Mut(mt) => self.bind(&mt.pat, value, is_decl, true, range),
			Pattern::Discard => match value {
				PVal::Static(_) => Ok(()),
				// the value isn't kept, but making it may do something
				PVal::Dynamic(expr) => {
					let lhs = Sourced::new(Pattern::Discard, range);
					self.emit(stmt(Expr::Assign(Assign { op: None, lhs: Box::new(lhs), rhs: Box::new(expr) }), range))
				},
			},
			Pattern::Binding(var) if is_decl => {
				match value {
					PVal::Static(value) => {
						let value = match value {
							Value::Func(func) if func.name.is_none() => Value::Func(FuncValue { node: func.node, name: Some(var.clone()) }),
							value => value,
						};
						self.declare(var, State::Static(value), is_mut);
					},
					PVal::Dynamic(expr) => {
						self.emit(declaration(var, is_mut, expr, range))?;
						let slot = self.declare(var, State::Dynamic, is_mut);
						self.slots[slot].declared = true;
						self.slots[slot].placeholder = None;
					},
				}
				Ok(())
			},
			Pattern::Binding(var) => match self.slot_of(var) {
				Some(slot) => self.store(slot, value, range),
				None => {
					let rhs = self.to_expr(value, range)?;
					self.emit(stmt(Expr::Assign(Assign { op: None, lhs: Box::new(pattern.clone()), rhs: Box::new(rhs) }), range))
				},
			},
			Pattern::Deref(ptr) => match self.eval(ptr)? {
				PVal::Static(Value::Ptr(slot)) => self.store(slot, value, range),
				ptr => {
					let ptr = self.to_expr(ptr, range)?;
					let rhs = self.to_expr(value, range)?;
					let lhs = Sourced::new(Pattern::Deref(Box::new(ptr)), range);
					self.emit(stmt(Expr::Assign(Assign { op: None, lhs: Box::new(lhs), rhs: Box::new(rhs) }), range))
				},
			},
			Pattern::TupleDtor(dtor) => match value {
				PVal::Static(Value::Tuple(items)) if items.len() == dtor.items.len() => {
					for (item, value) in dtor.items.iter().zip(items) {
						self.bind(item, PVal::Static(value), is_decl, is_mut, range)?;
					}
					Ok(())
				},
				value => {
					// destructured at runtime, so every binding in it lives at runtime
					let rhs = self.to_expr(value, range)?;
					if is_decl {
						self.declare_runtime_pattern(pattern);
					} else {
						self.dynamize_pattern(pattern, range)?;
					}
					let lhs = if is_decl {
						let inner = if is_mut { Sourced::new(Pattern::Mut(Mut { pat: Box::new(pattern.clone()) }), range) } else { pattern.clone() };
						Sourced::new(Pattern::Let(Let { pat: Box::new(inner) }), range)
					} else {
						pattern.clone()
					};
					self.emit(stmt(Expr::Assign(Assign { op: None, lhs: Box::new(lhs), rhs: Box::new(rhs) }), range))
				},
			},
		}
	}

	fn dynamize_pattern(&mut self, pattern: &'a Sourced<Pattern>, range: source::Range) -> R<'a, ()> {
		match &**pattern {
			Pattern::Binding(var) => if let Some(slot) = self.slot_of(var) {
				self.dynamize(slot, range)?;
				self.write(slot, State::Dynamic);
			},
			Pattern::TupleDtor(dtor) => for item in dtor.items.iter() {
				self.dynamize_pattern(item, range)?;
			},
			Pattern::Let(_) | Pattern::Mut(_) | Pattern::Deref(_) | Pattern::Discard => { },
		}
		Ok(())
	}

	/// Assigns to an existing variable.
	fn store(&mut self, slot: usize, value: PVal<'a>, range: source::Range) -> R<'a, ()> {
		match (self.slots[slot].state.clone(), value) {
			(State::Static(current), PVal::Static(value)) => {
				// a comptime value takes the variable's size
				let value = self.check(value::implicit_cast(&value, &current.ty()), range)?.unwrap_or(value);
				self.write(slot, State::Static(value));
				Ok(())
			},
			(_, value) => {
				self.dynamize(slot, range)?;
				self.write(slot, State::Dynamic);
				let rhs = self.to_expr(value, range)?;
				let var = self.slots[slot].var.clone();
				let lhs = Sourced::new(Pattern::Binding(var), range);
				self.emit(stmt(Expr::Assign(Assign { op: None, lhs: Box::new(lhs), rhs: Box::new(rhs) }), range))
			},
		}
	}
}

trait InnerRef<T> {
	fn inner_ref(&self) -> &T;
}

impl<T> InnerRef<T> for Sourced<T> {
	fn inner_ref(&self) -> &T {
		self
	}
}
