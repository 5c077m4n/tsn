use anyhow::{bail, Result};

use super::{
	ast::{
		BlockStmt, BooleanExpr, Expression, ExpressionStmt, FunctionLiteralExp, IdentifierExpr,
		IfExpr, InfixExpr, IntegerExpr, LetStmt, PrefixExpr, Program, ReturnStmt, Statement,
	},
	lexer::Lexer,
	token::{Token, TokenType},
};

static PREFIX_TOKEN_TYPES: &[TokenType; 8] = &[
	TokenType::Plus,
	TokenType::Minus,
	TokenType::Slash,
	TokenType::Asterisk,
	TokenType::DoubleEqual,
	TokenType::NEq,
	TokenType::LessThan,
	TokenType::GreaterThan,
];

#[derive(PartialEq, Eq, PartialOrd, Ord, Default)]
enum Precedence {
	#[default]
	Lowest,
	/// `==`
	Equals,
	/// `>` or `<`
	LessOrGreater,
	/// `+` or `-`
	Sum,
	/// `*` of `/`
	Product,
	/// `-X` or `!X`
	Prefix,
	/// `fnCall(X)`
	Call,
}
impl From<TokenType> for Precedence {
	fn from(value: TokenType) -> Self {
		match value {
			TokenType::DoubleEqual | TokenType::NEq => Self::Equals,
			TokenType::LessThan | TokenType::GreaterThan => Self::LessOrGreater,
			TokenType::Plus | TokenType::Minus => Self::Sum,
			TokenType::Asterisk | TokenType::Slash => Self::Product,
			TokenType::OpenParens => Self::Call,
			_ => Self::default(),
		}
	}
}

pub struct Parser {
	lexer: Box<Lexer>,
	errors: Vec<String>,
	token_current: Option<Token>,
	token_peek: Option<Token>,
}
impl Parser {
	fn next_token(&mut self) {
		self.token_current = self.token_peek.to_owned();
		self.token_peek = self.lexer.next().map(|td| td.token().clone());
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

	fn get_current_token(&self) -> Result<&Token> {
		let Some(token) = self.token_current.as_ref() else {
			bail!("The current token is empty");
		};
		Ok(token)
	}
	fn get_peek_token(&self) -> Result<&Token> {
		let Some(token) = self.token_peek.as_ref() else {
			bail!("The peek token is empty");
		};
		Ok(token)
	}

	fn current_precedence(&self) -> Result<Precedence> {
		let token = self.get_current_token()?;
		Ok(Precedence::from(TokenType::from(token)))
	}
	fn peek_precedence(&self) -> Result<Precedence> {
		let token = self.get_peek_token()?;
		Ok(Precedence::from(TokenType::from(token)))
	}

	fn infix_parse(&mut self, left: Box<Expression>) -> Result<Box<Expression>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		let precedence = self.current_precedence()?;
		self.next_token();

		let right = self.parse_expression(precedence)?;
		let infix_expr = InfixExpr {
			token: token.clone(),
			left,
			op: token.to_string(),
			right,
		};

		infix_expr.into()
	}
	fn prefix_parse(&mut self, _precedence: Precedence) -> Result<Box<Expression>> {
		let token = self.get_current_token()?;

		match TokenType::from(token) {
			TokenType::Identifier => {
				let token = token.clone();

				let value = token.to_string();
				let ident_expr = IdentifierExpr { token, value };

				ident_expr.into()
			}
			TokenType::Integer => {
				let token = token.clone();

				let value = token.to_string().parse::<usize>()?;
				let int_literal = IntegerExpr { token, value };

				int_literal.into()
			}
			TokenType::Bang | TokenType::Minus => {
				let token = self.get_current_token()?;
				let token = token.clone();

				self.next_token();

				let right = self.parse_expression(Precedence::Prefix)?;
				let prefix_expr = PrefixExpr {
					token: token.clone(),
					op: token.to_string(),
					right,
				};

				prefix_expr.into()
			}
			TokenType::True | TokenType::False => {
				let bool_expr = BooleanExpr {
					token: token.clone(),
					value: self.current_token_is(TokenType::True),
				};
				bool_expr.into()
			}
			TokenType::OpenParens => {
				self.next_token();
				let expr = self.parse_expression(Precedence::default());

				if !self.expect_peek(TokenType::CloseParens) {
					bail!("Exected a `)` here but got a `{}`", self.get_peek_token()?)
				}

				expr
			}
			TokenType::If => self.parse_if_expression(),
			TokenType::Function => self.parse_function_literal(),
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

	fn peek_error(&mut self, t: TokenType) {
		let msg = format!(
			"Expected the next token to be `{:?}`, but got `{:?}` instead",
			t, self.token_peek
		);
		self.errors.push(msg);
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

	fn parse_let_statement(&mut self) -> Result<Box<Statement>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		if !self.expect_peek(TokenType::Identifier) {
			bail!(
				"Unexpected token, recieved `{:?}` instead of an identifier",
				self.token_peek
			)
		}
		let current_token = self.get_current_token()?;
		let name = IdentifierExpr {
			token: current_token.clone(),
			value: current_token.to_string(),
		};
		let let_stmt = LetStmt {
			token,
			name,
			value: None,
		};

		if !self.expect_peek(TokenType::Equal) {
			bail!(
				"Unexpected token, recieved `{:?}` instead of an `=` sign",
				self.token_peek
			);
		}

		// TODO: remove this token skipping
		while self.token_current.is_some() && !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		let_stmt.into()
	}

	fn parse_return_statement(&mut self) -> Result<Box<Statement>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		let ret_stmt = ReturnStmt {
			token,
			return_value: None,
		};

		self.next_token();

		// TODO: remove this token skipping
		while self.token_current.is_some() && !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		ret_stmt.into()
	}

	fn parse_block_statement(&mut self) -> Result<Box<BlockStmt>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		let mut block = BlockStmt {
			token,
			statements: vec![],
		};
		self.next_token();

		while self.token_current.is_some() && !self.current_token_is(TokenType::CloseCurlyBraces) {
			let stmt = self.parse_statement()?;
			block.statements.push(*stmt);

			self.next_token();
		}

		Ok(Box::new(block))
	}

	fn parse_if_expression(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		if !self.expect_peek(TokenType::OpenParens) {
			bail!("Expected a `(` token here")
		}

		self.next_token();
		let cond = self.parse_expression(Precedence::default())?;

		if !self.expect_peek(TokenType::CloseParens) {
			bail!("Expected a `)` token here")
		}
		if !self.expect_peek(TokenType::OpenCurlyBraces) {
			bail!("Expected a `{{` token here")
		}

		let then = self.parse_block_statement()?;
		let mut expr = IfExpr {
			token,
			cond,
			then,
			alt: None,
		};

		if self.peek_token_is(TokenType::Else) {
			self.next_token();
			if !self.expect_peek(TokenType::OpenCurlyBraces) {
				bail!("Expected a `{{` token here")
			}

			expr.alt = Some(self.parse_block_statement()?);
		}

		expr.into()
	}

	fn parse_function_params(&mut self) -> Result<Vec<IdentifierExpr>> {
		let mut idents: Vec<IdentifierExpr> = vec![];

		if self.peek_token_is(TokenType::CloseParens) {
			self.next_token();
			return Ok(idents);
		}

		self.next_token();

		let token = self.get_current_token()?;
		let ident = IdentifierExpr {
			token: token.clone(),
			value: token.clone().to_string(),
		};
		idents.push(ident);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();

			let token = self.get_current_token()?;
			let ident = IdentifierExpr {
				token: token.clone(),
				value: token.clone().to_string(),
			};
			idents.push(ident);
		}

		if !self.expect_peek(TokenType::CloseParens) {
			bail!(
				"Expected a `)` token, but got `{}` instead",
				self.get_current_token()?
			)
		}

		Ok(idents)
	}

	fn parse_function_literal(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		if !self.expect_peek(TokenType::OpenParens) {
			bail!(
				"Expected a `(` but got a `{}` instead",
				self.get_peek_token()?
			);
		}

		let params = self.parse_function_params()?;

		if !self.expect_peek(TokenType::CloseParens) {
			bail!(
				"Expected a `)` but got a `{}` instead",
				self.get_peek_token()?
			);
		}

		let body = self.parse_block_statement()?;
		let fn_lit = FunctionLiteralExp {
			token,
			params,
			body,
		};

		fn_lit.into()
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<Expression>> {
		let mut left_expr = self.prefix_parse(Precedence::default())?;

		while {
			if let Some(ref t) = self.token_current {
				let t_type = TokenType::from(t);
				!PREFIX_TOKEN_TYPES.contains(&t_type)
					&& !self.peek_token_is(TokenType::Semicolon)
					&& self.peek_precedence()? > precedence
			} else {
				false
			}
		} {
			self.next_token();
			left_expr = self.infix_parse(left_expr)?;
		}
		Ok(left_expr)
	}

	fn parse_expression_statement(&mut self) -> Result<Box<Statement>> {
		let token = self.get_current_token()?;
		let token = token.clone();

		let mut expr_stmt = ExpressionStmt {
			token,
			expression: None,
		};

		let expr = self.parse_expression(Precedence::default())?;
		expr_stmt.expression = Some(expr);

		// TODO: remove this token skipping
		while self.token_current.is_some() && !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		expr_stmt.into()
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
		let mut program = Box::<Program>::default();

		while self.token_current.is_some() {
			match self.parse_statement() {
				Ok(s) => program.statements.push(*s),
				Err(msg) => self.errors.push(msg.to_string()),
			};
			self.next_token();
		}
		Ok(program)
	}
}

#[cfg(test)]
mod tests;
