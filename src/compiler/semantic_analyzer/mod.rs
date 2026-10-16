mod error;
mod types;
#[cfg(test)]
mod tests;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use error::{SemError, SemErrorKind};
pub use types::Type;

use crate::compiler::{
	lexer::literal::IntType,
	parser::{node_sets::*, nodes::*, Var},
	source::{self, Sourced},
};

/// Resolves every symbol to its declaration and checks the types, in one pass.
/// Gives back the type of the last statement in the file, like `u8` for `let a = 1; a + 1u8`.
pub fn analyze(ast: &mut Sourced<Expr>) -> Result<Type, Vec<SemError>> {
	let mut analyzer = Analyzer {
		scope: SymTbl::new(),
		loops: Vec::new(),
		errors: Vec::new(),
		last_stmt: Type::Nothing,
	};
	let ty = analyzer.expr(ast);
	let ty = match &**ast {
		Expr::Block(block) if block.expr.is_none() && !block.stmts.is_empty() => analyzer.last_stmt,
		_ => ty,
	};
	if analyzer.errors.is_empty() {
		Ok(ty)
	} else {
		Err(analyzer.errors)
	}
}

#[derive(Debug, Clone)]
struct Symbol {
	var: Rc<RefCell<Var>>,
	ty: Type,
}

#[derive(Debug, Clone)]
struct SymTbl {
	symbols: HashMap<String, Symbol>,
}

impl SymTbl {
	fn new() -> Self {
		let mut sym_tbl = Self {
			symbols: HashMap::new(),
		};
		//TODO: remove temp funcs. they're untyped until externs can be declared
		for name in ["printf", "scanf"] {
			sym_tbl.declare(Rc::new(RefCell::new(Var::new(name.to_string()))), false, Type::Unknown);
		}
		sym_tbl
	}

	fn declare(&mut self, var: Rc<RefCell<Var>>, is_mut: bool, ty: Type) {
		let name = {
			let mut var = var.borrow_mut();
			var.is_mut = is_mut;
			var.id = self.symbols.len() as u64;
			var.name.clone()
		};
		self.symbols.insert(name, Symbol { var, ty });
	}

	/// Links a use of a symbol to its declaration.
	fn resolve(&self, var: &mut Rc<RefCell<Var>>) -> Option<Type> {
		let symbol = self.symbols.get(&var.borrow().name)?;
		*var = symbol.var.clone();
		Some(symbol.ty.clone())
	}
}

enum LoopKind {
	/// A `loop`, whose type is the type of its `break`s.
	Loop(Option<Type>),
	/// A `while` or `for`, which can only break without a value.
	While,
}

struct Analyzer {
	scope: SymTbl,
	loops: Vec<LoopKind>,
	errors: Vec<SemError>,
	/// The type of the last statement analyzed in a block. After a whole file, that's the file's last statement.
	last_stmt: Type,
}

impl Analyzer {
	fn error(&mut self, kind: SemErrorKind, range: source::Range) {
		self.errors.push(SemError { kind, range });
	}

	/// Runs `f` in a new scope, so the symbols it declares are forgotten afterwards.
	fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
		let scope = self.scope.clone();
		let result = f(self);
		self.scope = scope;
		result
	}

	/// Reports an error unless `found` implicitly casts to `expected`.
	fn expect(&mut self, node: &Sourced<Expr>, found: &Type, expected: &Type) {
		if !found.implicitly_casts_to(expected) {
			self.error(SemErrorKind::Mismatch { expected: expected.clone(), found: found.clone() }, *node.source());
		} else {
			self.check_literal_fits(node, expected);
		}
	}

	/// Analyzes an expression and reports an error unless it implicitly casts to `expected`.
	fn expr_expecting(&mut self, node: &mut Sourced<Expr>, expected: &Type) -> Type {
		let found = self.expr(node);
		self.expect(node, &found, expected);
		found
	}

	/// Analyzes a body whose value is dropped, like a `while`'s, in its own scope.
	fn expr_discarded(&mut self, node: &mut Sourced<Expr>) {
		let ty = self.scoped(|this| this.expr(node));
		if !ty.is_discardable() {
			self.error(SemErrorKind::UncaughtValue(ty), *node.source());
		}
	}

	/// Comptime int literals know their value, so they can be checked against the type they're cast to.
	fn check_literal_fits(&mut self, node: &Sourced<Expr>, ty: &Type) {
		let (negative, lit) = match &**node {
			Expr::IntLit(lit) => (false, lit),
			Expr::UnaOp(UnaOp { op: UnaOpKind::Negate, expr }) => match &***expr {
				Expr::IntLit(lit) => (true, lit),
				_ => return,
			},
			_ => return,
		};
		if lit.ty.is_some() {
			return;
		}

		let value_bits = lit.value.bits();
		let fits = match ty {
			Type::Int { signed: false, bits } | Type::Bool(bits) => !negative && value_bits <= *bits,
			Type::Usize => !negative && value_bits <= types::POINTER_BITS,
			Type::Int { signed: true, bits } => fits_signed(&lit.value, negative, *bits),
			Type::Isize => fits_signed(&lit.value, negative, types::POINTER_BITS),
			_ => true,
		};
		if !fits {
			let value = format!("{}{}", if negative { "-" } else { "" }, lit.value);
			self.error(SemErrorKind::LiteralDoesNotFit { value, ty: ty.clone() }, *node.source());
		}
	}

	fn resolve_type(&mut self, node: &Sourced<TypeExpr>) -> Type {
		match &**node {
			TypeExpr::Named(name) => Type::from_name(name).unwrap_or_else(|| {
				self.error(SemErrorKind::UnknownType(name.clone()), *node.source());
				Type::Unknown
			}),
			TypeExpr::Tuple(items) => Type::Tuple(items.iter().map(|item| self.resolve_type(item)).collect()),
		}
	}

	fn expr(&mut self, node: &mut Sourced<Expr>) -> Type {
		let range = *node.source();
		match &mut **node {
			Expr::Assign(assign) => {
				//NOTE: order is important! otherwise things can get defined after they're used.
				let rhs = self.expr(&mut assign.rhs);
				match assign.op {
					None => {
						self.pattern(&mut assign.lhs, &rhs, Some(&assign.rhs), false, false);
					},
					Some(op) => {
						let lhs = self.pattern_type(&mut assign.lhs);
						let result = self.bin_op(op, &lhs, &rhs, range);
						if !result.implicitly_casts_to(&lhs) {
							self.error(SemErrorKind::Mismatch { expected: lhs.clone(), found: result }, *assign.rhs.source());
						} else {
							self.check_literal_fits(&assign.rhs, &lhs);
						}
					},
				}
				Type::VOID
			},
			Expr::Return(ret) => {
				if let Some(expr) = &mut ret.expr {
					self.expr(expr);
				}
				Type::Never
			},
			Expr::Break(brk) => {
				let ty = brk.expr.as_mut().map_or(Type::VOID, |expr| self.expr(expr));
				match self.loops.last_mut() {
					None => self.error(SemErrorKind::BreakOutsideLoop, range),
					Some(LoopKind::While) => if ty != Type::VOID && ty != Type::Never {
						self.error(SemErrorKind::BreakValueInWhile(ty), range);
					},
					Some(LoopKind::Loop(loop_ty)) => {
						let unified = match loop_ty {
							None => Some(ty.clone()),
							Some(loop_ty) => loop_ty.unify(&ty),
						};
						match unified {
							Some(unified) => *loop_ty = Some(unified),
							None => {
								let expected = loop_ty.clone().expect("unify only fails with a type");
								self.error(SemErrorKind::Mismatch { expected, found: ty }, range);
							},
						}
					},
				}
				Type::Never
			},
			Expr::Continue => {
				if self.loops.is_empty() {
					self.error(SemErrorKind::BreakOutsideLoop, range);
				}
				Type::Never
			},
			Expr::Unreachable => Type::Never,

			Expr::Plex(_) => Type::Unknown, //TODO: plex

			Expr::Loop(lp) => {
				self.loops.push(LoopKind::Loop(None));
				self.expr_discarded(&mut lp.body);
				let Some(LoopKind::Loop(ty)) = self.loops.pop() else { unreachable!("loops are pushed and popped in pairs") };
				ty.unwrap_or(Type::Never) // a loop without a break never ends
			},
			Expr::While(wh) => {
				self.expr_expecting(&mut wh.cond, &Type::Bool(1));
				self.loops.push(LoopKind::While);
				self.expr_discarded(&mut wh.body);
				self.loops.pop();
				if let Some(body_else) = &mut wh.body_else {
					self.expr_discarded(body_else);
				}
				Type::VOID
			},
			Expr::For(fr) => {
				self.expr(&mut fr.iter); //TODO: iterators
				self.scoped(|this| {
					this.pattern(&mut fr.binding, &Type::Unknown, None, true, false);
					this.loops.push(LoopKind::While);
					this.expr_discarded(&mut fr.body);
					this.loops.pop();
				});
				if let Some(body_else) = &mut fr.body_else {
					self.expr_discarded(body_else);
				}
				Type::VOID
			},
			Expr::If(node_if) => {
				self.expr_expecting(&mut node_if.cond, &Type::Bool(1));
				match &mut node_if.body_else {
					None => {
						self.expr_discarded(&mut node_if.body);
						Type::VOID
					},
					Some(body_else) => {
						let body = self.scoped(|this| this.expr(&mut node_if.body));
						let other = self.scoped(|this| this.expr(body_else));
						body.unify(&other).unwrap_or_else(|| {
							self.error(SemErrorKind::Mismatch { expected: body, found: other }, *body_else.source());
							Type::Unknown
						})
					},
				}
			},

			Expr::Block(block) => self.scoped(|this| {
				let mut diverges = false;
				for stmt in block.stmts.iter_mut() {
					let ty = this.expr(stmt);
					diverges |= ty == Type::Never;
					this.last_stmt = ty;
				}
				let ty = block.expr.as_mut().map_or(Type::Nothing, |expr| this.expr(expr));
				// a block that always hits a `ret` or `break` never gives a value
				if diverges { Type::Never } else { ty }
			}),
			Expr::Stmt(stmt) => {
				// `;` takes a VOID and gives a NOTHING
				let ty = self.expr(&mut stmt.expr);
				match ty {
					Type::Never => Type::Never,
					ty if ty.is_discardable() => Type::Nothing,
					ty => {
						self.error(SemErrorKind::UncaughtValue(ty), *stmt.expr.source());
						Type::Nothing
					},
				}
			},
			Expr::BinOp(bin_op) => {
				let lhs = self.expr(&mut bin_op.lhs);
				let rhs = self.expr(&mut bin_op.rhs);
				let ty = self.bin_op(bin_op.op, &lhs, &rhs, range);
				// the shift amount doesn't share the operand type, and comparisons give a b1 instead of it
				if !matches!(bin_op.op, BinOpKind::Bitwise(_)) && let Some(operand) = lhs.unify(&rhs) {
					self.check_literal_fits(&bin_op.lhs, &operand);
					self.check_literal_fits(&bin_op.rhs, &operand);
				}
				ty
			},
			Expr::UnaOp(una_op) => {
				let ty = self.expr(&mut una_op.expr);
				self.una_op(una_op.op, ty, range)
			},
			Expr::FieldAccess(field_access) => {
				let ty = self.expr(&mut field_access.expr);
				if ty != Type::Unknown {
					self.error(SemErrorKind::NoFields(ty), range); //TODO: fields on ints and floats, like f32.mantissa
				}
				Type::Unknown
			},
			Expr::Func(func) => {
				// functions don't see the symbols or loops around them
				let scope = std::mem::replace(&mut self.scope, SymTbl::new());
				let loops = std::mem::take(&mut self.loops);
				let arg = self.pattern(&mut func.binding, &Type::Unknown, None, true, false); //TODO: argument types
				let ret = self.expr(&mut func.body);
				self.scope = scope;
				self.loops = loops;
				Type::Func { arg: Box::new(arg), ret: Box::new(ret) }
			},
			Expr::Call(call) => {
				let func = self.expr(&mut call.func);
				match func {
					Type::Func { arg, ret } => {
						self.expr_expecting(&mut call.arg, &arg);
						*ret
					},
					Type::Unknown => {
						self.expr(&mut call.arg);
						Type::Unknown
					},
					func => {
						self.expr(&mut call.arg);
						self.error(SemErrorKind::NotCallable(func), *call.func.source());
						Type::Unknown
					},
				}
			},
			Expr::Cast(cast) => {
				let from = self.expr(&mut cast.expr);
				let to = self.resolve_type(&cast.ty);
				if !from.explicitly_casts_to(&to) {
					self.error(SemErrorKind::InvalidCast { from, to: to.clone() }, range);
				} else {
					self.check_literal_fits(&cast.expr, &to);
				}
				to
			},

			Expr::TupleCtor(tuple) => Type::Tuple(tuple.items.iter_mut().map(|item| self.expr(item)).collect()),
			Expr::BoolLit(_) => Type::BoolLit,
			Expr::IntLit(lit) => match lit.ty {
				None => Type::IntLit,
				Some(IntType::Signed(bits)) => Type::Int { signed: true, bits },
				Some(IntType::Unsigned(bits)) => Type::Int { signed: false, bits },
				Some(IntType::Bool(bits)) => Type::Bool(bits),
			},
			Expr::RealLit(lit) => lit.num_bits.map_or(Type::RealLit, Type::Float),
			Expr::StrLit(_) => Type::Str,
			Expr::Identifier(var) => self.resolve(var, range),
		}
	}

	fn resolve(&mut self, var: &mut Rc<RefCell<Var>>, range: source::Range) -> Type {
		self.scope.resolve(var).unwrap_or_else(|| {
			self.error(SemErrorKind::UndefinedSymbol(var.borrow().name.clone()), range);
			Type::Unknown
		})
	}

	/// Resolves an assignment target, which has to be mutable.
	fn resolve_mut(&mut self, var: &mut Rc<RefCell<Var>>, range: source::Range) -> Type {
		let ty = self.resolve(var, range);
		let var = var.borrow();
		// an undefined symbol is already reported
		if self.scope.symbols.contains_key(&var.name) && !var.is_mut {
			self.error(SemErrorKind::AssignImmutableSymbol(var.name.clone()), range);
		}
		ty
	}

	fn bin_op(&mut self, op: BinOpKind, lhs: &Type, rhs: &Type, range: source::Range) -> Type {
		let invalid = |this: &mut Self| {
			this.error(SemErrorKind::InvalidBinOp { op: op.to_string(), lhs: lhs.clone(), rhs: rhs.clone() }, range);
			Type::Unknown
		};

		if let BinOpKind::Bitwise(_) = op {
			// the shift amount is a count, so it doesn't share the type of the bits
			if !matches!(lhs, Type::Bool(_) | Type::Unknown | Type::Never) {
				self.error(SemErrorKind::ShiftNonBool(lhs.clone()), range);
				return Type::Unknown;
			}
			if !rhs.is_int() && !matches!(rhs, Type::Unknown | Type::Never) {
				return invalid(self);
			}
			return lhs.clone();
		}

		let Some(ty) = lhs.unify(rhs) else {
			return invalid(self);
		};
		let is_unknown = matches!(ty, Type::Unknown | Type::Never);
		match op {
			BinOpKind::Arith(_) if is_unknown || ty.is_numeric() => ty,
			BinOpKind::Cmp(CmpBinOpKind::Eq | CmpBinOpKind::Ne) => Type::Bool(1),
			BinOpKind::Cmp(_) if is_unknown || ty.is_numeric() => Type::Bool(1),
			BinOpKind::Logic(_) if is_unknown || ty.implicitly_casts_to(&Type::Bool(1)) => Type::Bool(1),
			_ => invalid(self),
		}
	}

	fn una_op(&mut self, op: UnaOpKind, ty: Type, range: source::Range) -> Type {
		let is_unknown = matches!(ty, Type::Unknown | Type::Never);
		let result = match (op, &ty) {
			(_, _) if is_unknown && op != UnaOpKind::AddressOf => Some(Type::Unknown),
			(UnaOpKind::Identity, ty) if ty.is_numeric() => Some(ty.clone()),
			(UnaOpKind::Negate, Type::Int { signed: false, .. } | Type::Usize) => None,
			(UnaOpKind::Negate, ty) if ty.is_numeric() => Some(ty.clone()),
			(UnaOpKind::Not, ty) if ty.is_bool() => Some(ty.clone()),
			(UnaOpKind::Deref, Type::Ptr(inner)) => Some((**inner).clone()),
			(UnaOpKind::Deref, ty) => {
				self.error(SemErrorKind::NotAPointer(ty.clone()), range);
				return Type::Unknown;
			},
			(UnaOpKind::AddressOf, ty) => Some(Type::Ptr(Box::new(ty.clone()))),
			_ => None,
		};
		result.unwrap_or_else(|| {
			self.error(SemErrorKind::InvalidUnaOp { op: op.to_string(), ty }, range);
			Type::Unknown
		})
	}

	/// The type of an existing assignment target, for compound assignments like `a += 1`.
	fn pattern_type(&mut self, node: &mut Sourced<Pattern>) -> Type {
		let range = *node.source();
		match &mut **node {
			Pattern::Binding(var) => self.resolve_mut(var, range),
			Pattern::Deref(expr) => {
				let ty = self.expr(expr);
				self.una_op(UnaOpKind::Deref, ty, range)
			},
			Pattern::TupleDtor(dtor) => Type::Tuple(dtor.items.iter_mut().map(|item| self.pattern_type(item)).collect()),
			Pattern::Let(_) | Pattern::Mut(_) | Pattern::Discard => {
				self.error(SemErrorKind::CompoundAssignDeclaration, range);
				Type::Unknown
			},
		}
	}

	/// Binds a pattern to a value of type `ty`, and gives back the type the pattern accepts.
	/// `value` is the expression being bound when there is one, so literals can be checked against the target.
	fn pattern(&mut self, node: &mut Sourced<Pattern>, ty: &Type, value: Option<&Sourced<Expr>>, is_decl: bool, is_mut: bool) -> Type {
		let range = *node.source();
		match &mut **node {
			Pattern::Let(lt) => {
				if is_decl {
					self.error(SemErrorKind::DoubleLet, range);
					return Type::Unknown;
				}
				debug_assert!(!is_mut);
				self.pattern(&mut lt.pat, ty, value, true, false)
			},
			Pattern::Mut(mt) => {
				if is_mut {
					self.error(SemErrorKind::DoubleMut, range);
					return Type::Unknown;
				}
				if !is_decl {
					self.error(SemErrorKind::MutWithoutLet, range);
					return Type::Unknown;
				}
				self.pattern(&mut mt.pat, ty, value, is_decl, true)
			},
			Pattern::Discard => ty.clone(),
			Pattern::Binding(var) => {
				if is_decl {
					if is_mut && ty.is_comptime() {
						self.error(SemErrorKind::MutComptime(ty.clone()), range);
					}
					self.scope.declare(var.clone(), is_mut, ty.clone());
					ty.clone()
				} else {
					let var_ty = self.resolve_mut(var, range);
					self.expect_assign(value, ty, &var_ty, range);
					var_ty
				}
			},
			Pattern::Deref(expr) => {
				let ptr = self.expr(expr);
				let target = self.una_op(UnaOpKind::Deref, ptr, range);
				self.expect_assign(value, ty, &target, range);
				target
			},
			Pattern::TupleDtor(dtor) => {
				let len = dtor.items.len();
				let items = match ty {
					Type::Tuple(items) if items.len() == len => items.clone(),
					Type::Unknown | Type::Never => vec![Type::Unknown; len],
					ty => {
						self.error(SemErrorKind::DestructureMismatch { found: ty.clone(), len }, range);
						vec![Type::Unknown; len]
					},
				};
				// a tuple constructor lines its items up with the pattern, so each literal can be checked
				let values = match value.map(|value| &**value) {
					Some(Expr::TupleCtor(tuple)) if tuple.items.len() == len => tuple.items.iter().map(Some).collect(),
					_ => vec![None; len],
				};
				Type::Tuple(dtor.items
					.iter_mut()
					.zip(items.iter())
					.zip(values)
					.map(|((item, ty), value)| self.pattern(item, ty, value, is_decl, is_mut))
					.collect())
			},
		}
	}

	/// Checks a value of type `ty` being assigned to a target of type `target`.
	fn expect_assign(&mut self, value: Option<&Sourced<Expr>>, ty: &Type, target: &Type, range: source::Range) {
		match value {
			Some(value) => self.expect(value, ty, target),
			None => if !ty.implicitly_casts_to(target) {
				self.error(SemErrorKind::Mismatch { expected: target.clone(), found: ty.clone() }, range);
			},
		}
	}
}

fn fits_signed(value: &num_bigint::BigUint, negative: bool, bits: u64) -> bool {
	if bits == 0 {
		return value.bits() == 0;
	}
	// i8 holds -128..=127
	let value_bits = value.bits();
	value_bits < bits || (negative && value_bits == bits && value.trailing_zeros() == Some(bits - 1))
}
