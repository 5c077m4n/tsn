use std::io::{stdin, stdout, Write};

use anyhow::Result;

use super::lexer::Lexer;

pub fn start() -> Result<()> {
	loop {
		print!(">>> ");
		let _ = stdout().lock().flush();

		let mut input = String::new();
		stdin().read_line(&mut input)?;

		let lexer = Lexer::new(input.into_boxed_str());
		for token in lexer.into_iter() {
			log::info!("{:?}", token);
		}
	}
}
