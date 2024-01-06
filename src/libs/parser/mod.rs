use anyhow::{bail, Result};

use super::{
	ast::{
		Expression,
		ExpressionStmt,
		IdentifierExpr,
		IntegerExpr,
		LetStmt,
		PrefixExpr,
		Program,
		ReturnStmt,
		Statement,
	},
	lexer::Lexer,
	token::{Token, TokenType},
};

enum Precedence {
	LOWEST = 0isize,
	/// `==`
	EQUALS,
	/// `>` or `<`
	LESSGREATER,
	/// `+`
	SUM,
	/// `*`
	PRODUCT,
	/// `-X` or `!X`
	PREFIX,
	/// `fnCall(X)`
	CALL,
}

pub struct Parser {
	lexer: Box<Lexer>,
	errors: Vec<String>,
	// token holders
	token_current: Option<Token>,
	token_peek: Option<Token>,
}
impl Parser {
	fn next_token(&mut self) {
		self.token_current = self.token_peek.to_owned();
		self.token_peek = self.lexer.next();
	}

	pub fn new(lexer: Box<Lexer>) -> Self {
		let mut p = Self {
			lexer,
			errors: vec![],

			token_current: None,
			token_peek: None,
		};

		p.next_token();
		p.next_token();
		p
	}

	fn prefix_parse(&mut self, _precedence: Precedence) -> Result<Box<Expression>> {
		let Some(ref token) = self.token_current else {
			bail!("The current token is empty");
		};

		match TokenType::from(token) {
			TokenType::Identifier => {
				let token = token.clone();

				let value = token.to_string();
				let ident = IdentifierExpr { token, value };

				let ident_expr = Expression::Identifier(ident);
				let ident_expr = Box::new(ident_expr);
				Ok(ident_expr)
			}
			TokenType::Integer => {
				let token = token.clone();

				let value = token.to_string().parse::<usize>()?;
				let int_literal = IntegerExpr { token, value };

				let int_expr = Expression::Integer(int_literal);
				let int_expr = Box::new(int_expr);
				Ok(int_expr)
			}
			TokenType::Bang | TokenType::Minus => {
				let mut prefix_expr = PrefixExpr {
					token: token.clone(),
					op: token.clone().to_string(),
					right: None,
				};

				self.next_token();

				let right = self.parse_expression(Precedence::PREFIX)?;
				prefix_expr.right = Some(right);

				let prefix_expr = Expression::Prefix(prefix_expr);
				let prefix_expr = Box::new(prefix_expr);
				Ok(prefix_expr)
			}
			other => bail!("No parsing fn exists for the `{:?}` token type", other),
		}
	}

	pub fn errors(&self) -> &[String] {
		&self.errors[..]
	}

	fn current_token_is(&self, t: TokenType) -> bool {
		self.token_current
			.as_ref()
			.is_some_and(|token| TokenType::from(token) == t)
	}
	fn peek_token_is(&self, t: TokenType) -> bool {
		self.token_peek
			.as_ref()
			.is_some_and(|token| TokenType::from(token) == t)
	}
	fn expect_peek(&mut self, t: TokenType) -> bool {
		let is_expeted_token = self.peek_token_is(t);
		if is_expeted_token {
			self.next_token();
		} else {
			self.peek_error(t);
		}
		is_expeted_token
	}
	fn peek_error(&mut self, t: TokenType) {
		self.errors.push(format!(
			"Expected the next token to be `{:?}`, but got `{:?}` instead",
			t, self.token_peek
		));
	}

	fn parse_let_statement(&mut self) -> Result<Box<Statement>> {
		let Some(ref token) = self.token_current else {
			bail!("The current token is empty");
		};
		let token = token.clone();

		let mut let_stmt = LetStmt {
			token,
			name: None,
			value: None,
		};
		if !self.expect_peek(TokenType::Identifier) {
			bail!(
				"Unexpected token, recieved `{:?}` instead of an identifier",
				self.token_peek
			)
		}
		let current_token = self.token_current.as_ref().unwrap();
		let name = IdentifierExpr {
			token: current_token.clone(),
			value: current_token.to_string(),
		};
		let_stmt.name = Some(name);

		if !self.expect_peek(TokenType::Eq) {
			bail!(
				"Unexpected token, recieved `{:?}` instead of an `=` sign",
				self.token_peek
			);
		}

		// TODO: remove this token skipping
		while !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		let let_stmt = Statement::Let(let_stmt);
		let let_stmt = Box::new(let_stmt);
		Ok(let_stmt)
	}

	fn parse_return_statement(&mut self) -> Result<Box<Statement>> {
		let Some(ref token) = self.token_current else {
			bail!("The current token is empty");
		};
		let token = token.clone();

		let ret_stmt = ReturnStmt {
			token,
			return_value: None,
		};

		self.next_token();

		// TODO: remove this token skipping
		while !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		let ret_stmt = Statement::Return(ret_stmt);
		let ret_stmt = Box::new(ret_stmt);
		Ok(ret_stmt)
	}

	fn parse_expression(&mut self, _precedence: Precedence) -> Result<Box<Expression>> {
		self.prefix_parse(Precedence::LOWEST)
	}

	fn parse_expression_statement(&mut self) -> Result<Box<Statement>> {
		let Some(ref token) = self.token_current else {
			bail!("The current token is empty");
		};
		let token = token.clone();

		let mut expr_stmt = ExpressionStmt {
			token,
			expression: None,
		};

		let expr = self.parse_expression(Precedence::LOWEST)?;
		expr_stmt.expression = Some(expr);

		// TODO: remove this token skipping
		while !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		let expr_stmt = Statement::Expression(expr_stmt);
		let expr_stmt = Box::new(expr_stmt);
		Ok(expr_stmt)
	}

	fn parse_statement(&mut self) -> Result<Box<Statement>> {
		match &self.token_current {
			Some(Token::Let) => self.parse_let_statement(),
			Some(Token::Return) => self.parse_return_statement(),
			None => bail!("The current token should not be empty here"),
			_ => self.parse_expression_statement(),
		}
	}

	pub fn parse_program(&mut self) -> Result<Box<Program>> {
		let mut program = Box::new(Program::default());

		while self.token_current.is_some() {
			if let Ok(s) = self.parse_statement() {
				program.statements.push(s);
			}

			self.next_token();
		}
		Ok(program)
	}
}

#[cfg(test)]
mod tests;
