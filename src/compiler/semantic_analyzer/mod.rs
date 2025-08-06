use std::{cell::RefCell, collections::HashMap, ops::DerefMut, rc::Rc};

use crate::compiler::{parser::{node_sets::*, Var}, source::Sourced};
use error::SemError;

mod error;

#[derive(Debug, Clone)]
struct SymTbl {
	symbols: HashMap<String, Rc<RefCell<Var>>>,
}

impl SymTbl {
	pub fn new() -> Self {
		Self {
			symbols: HashMap::new(),
		}
	}

	fn declare(&mut self, var: Rc<RefCell<Var>>, is_mut: bool) {
		let name = {
			let mut var = var.borrow_mut();
			var.is_mut = is_mut;
			var.id = self.symbols.len() as u64;
			var.name.clone()
		};
		self.symbols.insert(name, var);
	}
	
	fn get(&self, var: &mut Rc<RefCell<Var>>) -> Result<(), SemError> {
		let sym_var = self.symbols
			.get(&var.borrow().name)
			.ok_or_else(|| SemError::UndefinedSymbol(var.clone()))?;
		*var = sym_var.clone();
		Ok(())
	}
}

pub fn analyze(ast: &mut Sourced<Expr>) -> Result<(), Vec<SemError>> {
	let mut sym_tbl = SymTbl::new();
	sym_tbl.declare(Rc::new(RefCell::new(Var { name: "print".to_string(), id: 0, is_mut: false })), false); //TODO: remove temp func

	let mut errors = Vec::new();
	eval_expr(ast, &mut sym_tbl, &mut errors);
	if errors.is_empty() {
		Ok(())
	} else {
		Err(errors)
	}
}

fn eval_expr(node: &mut Sourced<Expr>, sym_tbl: &mut SymTbl, errors: &mut Vec<SemError>) {
	match node.deref_mut() {
		Expr::Assign(value) => {
			//NOTE: order is important! otherwise things can get defined after they're used.
			eval_expr(&mut value.rhs, sym_tbl, errors);
			eval_pattern(&mut value.lhs, sym_tbl, errors, false, false);
		},
		Expr::Return(value) => if let Some(expr) = &mut value.expr { eval_expr(expr, sym_tbl, errors) }
		Expr::Break(value) => if let Some(expr) = &mut value.expr { eval_expr(expr, sym_tbl, errors) }
		Expr::Continue => { },
		Expr::Unreachable => { },
		Expr::Plex(value) => todo!("{:?}", value),
		Expr::Loop(value) => {
			eval_expr(&mut value.body, &mut sym_tbl.clone(), errors);
			if let Some(body_else) = &mut value.body_else {
				eval_expr(body_else, &mut sym_tbl.clone(), errors)
			}
		}
		Expr::While(value) => {
			eval_expr(&mut value.cond, sym_tbl, errors);
			eval_expr(&mut value.body, &mut sym_tbl.clone(), errors);
			if let Some(body_else) = &mut value.body_else {
				eval_expr(body_else, &mut sym_tbl.clone(), errors)
			}
		}
		Expr::For(value) => {
			eval_expr(&mut value.iter, sym_tbl, errors);
			let mut body_sym_tbl = sym_tbl.clone();
			eval_pattern(&mut value.binding, &mut body_sym_tbl, errors, true, false);
			eval_expr(&mut value.body, &mut body_sym_tbl, errors);
			if let Some(body_else) = &mut value.body_else {
				eval_expr(body_else, &mut sym_tbl.clone(), errors)
			}
		}
		Expr::If(value) => {
			eval_expr(&mut value.cond, sym_tbl, errors);
			eval_expr(&mut value.body, &mut sym_tbl.clone(), errors);
			if let Some(body_else) = &mut value.body_else {
				eval_expr(body_else, &mut sym_tbl.clone(), errors)
			}
		}

		Expr::Block(value) => {
			let mut sym_tbl = sym_tbl.clone();
			for stmt in value.stmts.iter_mut() {
				eval_expr(stmt, &mut sym_tbl, errors);
			}
		},
		Expr::Stmt(value) => eval_expr(&mut value.expr, sym_tbl, errors),
		Expr::BinOp(value) => {
			eval_expr(&mut value.lhs, sym_tbl, errors);
			eval_expr(&mut value.rhs, sym_tbl, errors);
		},
		Expr::UnaOp(value) => eval_expr(&mut value.expr, sym_tbl, errors),
		Expr::FieldAccess(value) => eval_expr(&mut value.expr, sym_tbl, errors),
		Expr::Func(value) => {
			eval_expr(&mut value.arg, sym_tbl, errors);
			eval_expr(&mut value.body, sym_tbl, errors);
		},
		Expr::Call(value) => {
			eval_expr(&mut value.func, sym_tbl, errors);
			eval_expr(&mut value.arg, sym_tbl, errors);
		},

		Expr::TupleCtor(value) => {
			for item in value.items.iter_mut() {
				eval_expr(item, sym_tbl, errors);
			}
		},
		Expr::BoolLit(_value) => { },
		Expr::IntLit(_value) => { },
		Expr::StrLit(_value) => { },
		Expr::Identifier(var) => {
			if let Err(err) = sym_tbl.get(var) { errors.push(err); return; }
			//println!("USE:  {}", var.borrow());
		},
	}
}

fn eval_pattern(node: &mut Sourced<Pattern>, sym_tbl: &mut SymTbl, errors: &mut Vec<SemError>, is_decl: bool, is_mut: bool) {
	match node.deref_mut() {
		Pattern::Let(value) => {
			if is_decl { errors.push(SemError::DoubleLet); return; }
			debug_assert!(!is_mut);
			eval_pattern(&mut value.pat, sym_tbl, errors, true, false);
		},
		Pattern::Mut(value) => {
			if is_mut { errors.push(SemError::DoubleMut); return; }
			if !is_decl { errors.push(SemError::MutWithoutLet); return; }
			eval_pattern(&mut value.pat, sym_tbl, errors, is_decl, true);
		}
		Pattern::TupleDtor(value) => {
			for item in value.items.iter_mut() {
				eval_pattern(item, sym_tbl, errors, is_decl, is_mut);
			}
		},
		Pattern::Deref(value) => eval_expr(value, sym_tbl, errors),
		Pattern::Binding(var) => {
			if is_decl {
				sym_tbl.declare(var.clone(), is_mut);
				//println!("DECL: {}, mut:{}", var.borrow(), is_mut);
			} else {
				if let Err(err) = sym_tbl.get(var) { errors.push(err); return; }
				if !var.borrow().is_mut { errors.push(SemError::AssignImmutableSymbol(var.clone())); return; }
				//println!("SET:  {}", var.borrow());
			}
		},
	}
}
