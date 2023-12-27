use super::{ast::Program, lexer::Lexer, token::Token};

pub struct Parser {
	lexer: Box<Lexer>,
	token_current: Option<Token>,
	token_peek: Option<Token>,
}
impl Parser {
	fn next_token(&mut self) {
		self.token_current = self.token_peek.to_owned();
		self.token_peek = Some(self.lexer.next_token());
	}

	pub fn new(lexer: Box<Lexer>) -> Self {
		let mut p = Self {
			lexer,
			token_current: None,
			token_peek: None,
		};
		p.next_token();
		p.next_token();
		p
	}

	pub fn parse_program(&self) -> Box<Program> {
		Box::new(Program { statements: vec![] })
	}
}
