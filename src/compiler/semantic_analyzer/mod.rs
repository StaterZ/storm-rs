use std::{collections::HashMap, rc::Rc};

use color_print::cprintln;

use crate::compiler::parser::{self, Var};
use error::SemError;

mod error;

pub fn analyze(ast: &mut parser::nodes::Node<parser::nodes::Expr>) -> Result<(), SemError> {
	let mut symbols = HashMap::new();
	let result = eval_expr(ast, &mut symbols);
	match &result {
		Ok(_) => cprintln!("<yellow>HAPPY! ^v^</>"),
		Err(_) => cprintln!("<blue>SAD >~<<</>"),
	}
	result
}

fn eval_expr(node: &mut parser::nodes::Node<parser::nodes::Expr>, symbols: &mut HashMap<String, Rc<Var>>) -> Result<(), SemError> {
	match &mut node.kind {
		parser::nodes::Expr::Assign(value) => {
			//NOTE: order is important! otherwise things can get defined after they're used.
			eval_expr(&mut value.rhs, symbols)?;
			eval_pattern(&mut value.lhs, symbols, false, false)?;
		},
		parser::nodes::Expr::Return(value) => value.expr
			.as_mut()
			.map_or(Ok(()), |expr|
				eval_expr(expr, symbols))?,
		parser::nodes::Expr::Break(value) => value.expr
			.as_mut()
			.map_or(Ok(()), |expr|
				eval_expr(expr, symbols))?,
		parser::nodes::Expr::Continue => { },
		parser::nodes::Expr::Unreachable => { },
		
		parser::nodes::Expr::Loop(value) => {
			eval_expr(&mut value.body, &mut symbols.clone())?;
			value.body_else
				.as_mut()
				.map_or(Ok(()), |body_else|
					eval_expr(body_else, &mut symbols.clone()))?;
		}
		parser::nodes::Expr::While(value) => {
			eval_expr(&mut value.cond, symbols)?;
			eval_expr(&mut value.body, &mut symbols.clone())?;
			value.body_else
				.as_mut()
				.map_or(Ok(()), |body_else|
					eval_expr(body_else, &mut symbols.clone()))?;
		}
		parser::nodes::Expr::For(value) => {
			eval_expr(&mut value.iter, symbols)?;
			let mut body_symbols = symbols.clone();
			eval_pattern(&mut value.binding, &mut body_symbols, true, false)?;
			eval_expr(&mut value.body, &mut body_symbols)?;
			value.body_else
				.as_mut()
				.map_or(Ok(()), |body_else|
					eval_expr(body_else, &mut symbols.clone()))?;
		}
		parser::nodes::Expr::If(value) => {
			eval_expr(&mut value.cond, symbols)?;
			eval_expr(&mut value.body, &mut symbols.clone())?;
			value.body_else
				.as_mut()
				.map_or(Ok(()), |body_else|
					eval_expr(body_else, &mut symbols.clone()))?;
		}

		parser::nodes::Expr::Block(value) => {
			let mut symbols = symbols.clone();
			for stmt in value.stmts.iter_mut() {
				eval_expr(stmt, &mut symbols)?;
			}
		},
		parser::nodes::Expr::Stmt(value) => eval_expr(&mut value.expr, symbols)?,
		parser::nodes::Expr::BinOp(value) => {
			eval_expr(&mut value.lhs, symbols)?;
			eval_expr(&mut value.rhs, symbols)?;
		},
		parser::nodes::Expr::UnaOp(value) => eval_expr(&mut value.expr, symbols)?,
		parser::nodes::Expr::FieldAccess(value) => eval_expr(&mut value.expr, symbols)?,

		parser::nodes::Expr::TupleCtor(value) => {
			for item in value.items.iter_mut() {
				eval_expr(item, symbols)?;
			}
		},
		parser::nodes::Expr::IntLit(_value) => { },
		parser::nodes::Expr::StrLit(_value) => { },
		parser::nodes::Expr::Identifier(var) => {
			let sym_var = symbols
				.get(&var.name)
				.ok_or_else(|| SemError::UndefinedSymbol(var.clone()))?;
			*var = sym_var.clone();
			println!("var:{}, USE", var);
		},
	}
	Ok(())
}

fn eval_pattern(node: &mut parser::nodes::Node<parser::nodes::Pattern>, symbols: &mut HashMap<String, Rc<Var>>, is_decl: bool, is_mut: bool) -> Result<(), SemError> {
	match &mut node.kind {
		parser::nodes::Pattern::Let(value) => {
			if is_decl { return Err(SemError::DoubleLet); }
			debug_assert!(!is_mut);
			eval_pattern(&mut value.pat, symbols, true, false)?
		},
		parser::nodes::Pattern::Mut(value) => {
			if is_mut { return Err(SemError::DoubleMut); }
			if !is_decl { return Err(SemError::MutWithoutLet); }
			eval_pattern(&mut value.pat, symbols, is_decl, true)?
		}
		parser::nodes::Pattern::TupleDtor(value) => {
			for item in value.items.iter_mut() {
				eval_pattern(item, symbols, is_decl, is_mut)?;
			}
		},
		parser::nodes::Pattern::Deref(value) => eval_expr(value, symbols)?,
		parser::nodes::Pattern::Binding(var) => {
			symbols.insert(var.name.clone(), var.clone());
			println!("var:{}, decl:{} mut:{}", var, is_decl, is_mut);
		},
	}
	Ok(())
}
