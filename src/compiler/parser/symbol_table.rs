use std::{collections::HashMap, rc::Rc};

use super::Var;

#[derive(Debug)]
pub struct SymbolTable {
	scopes: Vec<HashMap<String, Rc<Var>>>,
}

impl SymbolTable {
	pub fn new() -> Self {
		Self {
			scopes: vec![
				HashMap::<String, Rc<Var>>::new(),
			],
		}
	}

	pub fn create_var(&mut self, var: Var) -> Rc<Var> {
		match self.scopes.last_mut().unwrap().try_insert(var.name.clone(), Rc::new(var)) {
			Ok(value) => value.clone(),
			Err(err) => err.value,
		}
	}

	pub fn push(&mut self) {
		let scope = self.scopes.last().unwrap(); //unwrap is safe since we'll always have a base scope to grab
		self.scopes.push(scope.clone());
	}

	pub fn pop(&mut self) -> bool {
		if self.scopes.is_empty() {
			return false;
		}

		self.scopes.pop();
		return true;
	}
}