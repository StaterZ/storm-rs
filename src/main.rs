#![allow(dead_code)]
#![feature(trait_alias)]
#![feature(map_try_insert)]
#![feature(type_alias_impl_trait)]
#![feature(const_trait_impl)]
#![feature(new_range_api)]

use std::error::Error;

mod driver;
mod lexer;
mod parser;

fn main() -> Result<(), Box<dyn Error>> {
	Ok(driver::main()?)
}
