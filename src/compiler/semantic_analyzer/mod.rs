use std::{collections::HashMap, rc::Rc};

use color_print::cprintln;

use crate::compiler::parser::{self, Var};
use error::SemError;

mod error;

pub fn analyze<'a>(ast: &'a parser::nodes::Node) -> Result<(), SemError<'a>> {
	let mut symbols = HashMap::new();
	let result = eval(ast, &mut symbols);
	match &result {
		Ok(_) => cprintln!("<yellow>HAPPY! ^v^</>"),
		Err(_) => cprintln!("<blue>SAD >~<<</>"),
	}
	result
}

fn eval<'a>(node: &'a parser::nodes::Node, symbols: &mut HashMap<String, Rc<Var>>) -> Result<(), SemError<'a>> {
	match &node.kind {
		parser::nodes::NodeKind::Assign(value) => eval_pattern(&value.lhs, symbols, false, false),
		parser::nodes::NodeKind::Block(value) => {
			for stmt in value.stmts.iter() {
				eval(stmt, &mut symbols.clone())?;
			}
			Ok(())
		},
		parser::nodes::NodeKind::Stmt(value) => eval(&value.expr, symbols),
		_ => Ok(()),
	}
}

fn eval_pattern<'a>(node: &'a parser::nodes::Node, symbols: &mut HashMap<String, Rc<Var>>, is_decl: bool, is_mut: bool) -> Result<(), SemError<'a>> {
	match &node.kind {
		parser::nodes::NodeKind::Let(value) => {
			if is_decl { return Err(SemError::DoubleLet); }
			debug_assert!(!is_mut);
			eval_pattern(&value.expr, symbols, true, false)?
		},
		parser::nodes::NodeKind::Mut(value) => {
			if is_mut { return Err(SemError::DoubleMut); }
			if !is_decl { return Err(SemError::MutWithoutLet); }
			eval_pattern(&value.expr, symbols, is_decl, true)?
		}
		parser::nodes::NodeKind::TupleDtor(value) => {
			for item in value.items.iter() {
				eval_pattern(item, symbols, is_decl, is_mut)?;
			}
		},
		parser::nodes::NodeKind::Deref(_value) => { }, //skip expr
		parser::nodes::NodeKind::Identifier(var) => {
			symbols.insert(var.name.clone(), var.clone());
			println!("var:{}, decl:{} mut:{}", &var, is_decl, is_mut);
		},
		_ => return Err(SemError::BadPattern(&node)),
	}
	Ok(())
}
