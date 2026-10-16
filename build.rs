fn main() {
	lalrpop::Configuration::new()
		.set_in_dir("src")
		.emit_rerun_directives(true)
		.process()
		.unwrap();
}
