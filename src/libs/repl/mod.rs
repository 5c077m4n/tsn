use std::io::{stdin, stdout, Write};

use anyhow::Result;

use super::{lexer::Lexer, parser::Parser};

pub fn start() -> Result<()> {
	loop {
		print!(">>> ");
		let _ = stdout().lock().flush();

		let mut input = String::new();
		stdin().read_line(&mut input)?;

		let lexer = Lexer::new(&input.into_boxed_str());
		let lexer = Box::new(lexer);
		let mut parser = Parser::new(lexer);
		let program = parser.parse_program()?;

		if !parser.errors().is_empty() {
			for error in parser.errors() {
				eprintln!("{}", error);
			}
		}
		println!("{}", program);
	}
}
