use anyhow::{bail, Result};

use super::{
	ast::{
		ArrayLiteralExpr, BlockStmt, BooleanExpr, Expression, ExpressionStmt, FunctionLiteralExpr,
		IdentifierExpr, IfExpr, InfixExpr, IntegerExpr, LetStmt, PrefixExpr, Program, ReturnStmt,
		Statement, StringExpr,
	},
	lexer::Lexer,
	token::{Token, TokenData, TokenType},
};

static PREFIX_TOKEN_TYPES: &[TokenType; 8] = &[
	TokenType::Plus,
	TokenType::Minus,
	TokenType::Slash,
	TokenType::Asterisk,
	TokenType::DoubleEqual,
	TokenType::NotEqual,
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
			TokenType::DoubleEqual | TokenType::NotEqual => Self::Equals,
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
	token_current: Option<TokenData>,
	token_peek: Option<TokenData>,
}
impl Parser {
	fn next_token(&mut self) {
		self.token_peek.clone_into(&mut self.token_current);
		self.token_peek = self.lexer.next();
	}

	pub fn new(lexer: Box<Lexer>) -> Self {
		let mut p = Self {
			lexer,
			errors: vec![],
			token_current: None,
			token_peek: None,
		};

		// This is to make sure that both the `token_current` and `token_peek` are filled
		p.next_token();
		p.next_token();
		p
	}

	fn get_current_token_data(&self) -> Result<&TokenData> {
		let Some(token_data) = self.token_current.as_ref() else {
			bail!("The current token is empty");
		};
		Ok(token_data)
	}
	fn get_peek_token_data(&self) -> Result<&TokenData> {
		let Some(token_data) = self.token_peek.as_ref() else {
			bail!("The peek token is empty");
		};
		Ok(token_data)
	}

	fn current_precedence(&self) -> Result<Precedence> {
		let token = self.get_current_token_data()?.token();
		Ok(Precedence::from(TokenType::from(token)))
	}
	fn peek_precedence(&self) -> Result<Precedence> {
		let token = self.get_peek_token_data()?.token();
		Ok(Precedence::from(TokenType::from(token)))
	}

	fn infix_parse(&mut self, left: Box<Expression>) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();
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

	fn parse_if_expression(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();

		self.assert_peek(TokenType::OpenParens)?;
		self.next_token();

		let cond = self.parse_expression(Precedence::default())?;
		self.assert_peek(TokenType::CloseParens)?;

		let then = self.parse_block_statement()?;
		let mut expr = IfExpr {
			token,
			cond,
			then,
			alt: None,
		};

		if self.peek_token_is(TokenType::Else) {
			self.next_token();
			expr.alt = Some(self.parse_block_statement()?);
		}

		expr.into()
	}

	fn parse_function_params(&mut self) -> Result<Vec<IdentifierExpr>> {
		self.assert_peek(TokenType::OpenParens)?;

		let mut idents: Vec<IdentifierExpr> = vec![];

		if self.peek_token_is(TokenType::CloseParens) {
			self.next_token();
			return Ok(idents);
		}

		self.next_token();

		let token = self.get_current_token_data()?.token();
		let ident = IdentifierExpr {
			token: token.clone(),
			value: token.clone().to_string(),
		};
		idents.push(ident);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();

			let token = self.get_current_token_data()?.token();
			let ident = IdentifierExpr {
				token: token.clone(),
				value: token.clone().to_string(),
			};
			idents.push(ident);
		}

		self.assert_peek(TokenType::CloseParens)?;

		Ok(idents)
	}

	fn parse_function_literal(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();

		let params = self.parse_function_params()?;
		let body = self.parse_block_statement()?;
		let fn_lit = FunctionLiteralExpr {
			token,
			params,
			body,
		};

		fn_lit.into()
	}

	fn parse_expression_list(&mut self, closing_token: TokenType) -> Result<Vec<Expression>> {
		let mut exprs: Vec<Expression> = vec![];

		if self.peek_token_is(closing_token) {
			return Ok(exprs);
		}
		self.next_token();

		exprs.push(*self.parse_expression(Precedence::default())?);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();
			exprs.push(*self.parse_expression(Precedence::default())?);
		}
		self.assert_peek(closing_token)?;

		Ok(exprs)
	}
	fn parse_array_literal(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();
		let array = ArrayLiteralExpr {
			token,
			elements: self.parse_expression_list(TokenType::CloseSquareBraces)?,
		};

		array.into()
	}

	fn prefix_parse(&mut self, _precedence: Precedence) -> Result<Box<Expression>> {
		let token_data = self.get_current_token_data()?;
		let token = token_data.token();

		match TokenType::from(token) {
			TokenType::True | TokenType::False => {
				let bool_expr = BooleanExpr {
					token: token.clone(),
					value: self.current_token_is(TokenType::True),
				};
				bool_expr.into()
			}
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
			TokenType::String => {
				let token = token.clone();

				let value = token.to_string();
				let str_literal = StringExpr { token, value };

				str_literal.into()
			}
			TokenType::Bang | TokenType::Minus => {
				let token = self.get_current_token_data()?.token().clone();
				self.next_token();

				let right = self.parse_expression(Precedence::Prefix)?;
				let prefix_expr = PrefixExpr {
					token: token.clone(),
					op: token.to_string(),
					right,
				};

				prefix_expr.into()
			}
			TokenType::OpenParens => {
				self.next_token();
				let expr = self.parse_expression(Precedence::default());

				self.assert_peek(TokenType::CloseParens)?;

				expr
			}
			TokenType::If => self.parse_if_expression(),
			TokenType::Function => self.parse_function_literal(),
			TokenType::OpenSquareBraces => self.parse_array_literal(),
			TokenType::OpenCurlyBraces => todo!("Object parser"),
			other => bail!(
				"No parsing function exists for the `{:?}` token type @ {}",
				other,
				token_data.location()
			),
		}
	}

	pub fn errors(&self) -> &[String] {
		&self.errors[..]
	}

	fn current_token_is(&self, t: TokenType) -> bool {
		self.token_current
			.as_ref()
			.is_some_and(|token_data| TokenType::from(token_data.token()) == t)
	}
	fn current_token_is_not(&self, t: TokenType) -> bool {
		self.token_current
			.as_ref()
			.is_some_and(|token_data| TokenType::from(token_data.token()) != t)
	}
	fn peek_token_is(&self, t: TokenType) -> bool {
		self.token_peek
			.as_ref()
			.is_some_and(|token_data| TokenType::from(token_data.token()) == t)
	}
	fn peek_token_is_not(&self, t: TokenType) -> bool {
		self.token_peek
			.as_ref()
			.is_some_and(|token_data| TokenType::from(token_data.token()) != t)
	}

	fn assert_peek(&mut self, t: TokenType) -> Result<()> {
		if let Some(token_data) = &self.token_peek {
			if TokenType::from(token_data.token()) != t {
				bail!(
					"Expected the next token to be `{:?}`, but got `{:?}` @ {} instead",
					t,
					token_data.token(),
					token_data.location()
				);
			} else {
				self.next_token();
				Ok(())
			}
		} else {
			bail!(
				"Expected the next token to be `{:?}`, but got nothing instead",
				t
			)
		}
	}

	fn parse_let_statement(&mut self) -> Result<Box<Statement>> {
		let let_token = self.get_current_token_data()?.token().clone();

		self.assert_peek(TokenType::Identifier)?;

		let ident_token = self.get_current_token_data()?.token();
		let name = IdentifierExpr {
			token: ident_token.clone(),
			value: ident_token.to_string(),
		};
		let mut let_stmt = LetStmt {
			token: let_token,
			name,
			value: None,
		};

		self.next_token();
		if self.current_token_is(TokenType::Equal) {
			self.next_token();
			let_stmt.value = Some(self.parse_expression(Precedence::default())?);
		}
		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		let_stmt.into()
	}

	fn parse_return_statement(&mut self) -> Result<Box<Statement>> {
		let return_token = self.get_current_token_data()?.token().clone();

		let mut ret_stmt = ReturnStmt {
			token: return_token,
			value: None,
		};
		self.next_token();

		if self.current_token_is_not(TokenType::Semicolon) {
			ret_stmt.value = Some(self.parse_expression(Precedence::default())?);
		}
		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		ret_stmt.into()
	}

	fn parse_block_statement(&mut self) -> Result<Box<BlockStmt>> {
		self.assert_peek(TokenType::OpenCurlyBraces)?;

		let token = self.get_current_token_data()?.token().clone();
		let mut block = BlockStmt {
			token,
			statements: vec![],
		};
		self.next_token();

		while self.current_token_is_not(TokenType::CloseCurlyBraces) {
			let stmt = self.parse_statement()?;
			block.statements.push(*stmt);

			self.next_token();
		}

		Ok(Box::new(block))
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<Expression>> {
		let mut left_expr = self.prefix_parse(Precedence::default())?;

		while {
			if let Some(ref token_data) = self.token_current {
				let token_type = TokenType::from(token_data.token());
				!PREFIX_TOKEN_TYPES.contains(&token_type)
					&& self.peek_token_is_not(TokenType::Semicolon)
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
		let token = self.get_current_token_data()?.token().clone();

		let mut expr_stmt = ExpressionStmt {
			token,
			expression: None,
		};

		let expr = self.parse_expression(Precedence::default())?;
		expr_stmt.expression = Some(expr);

		// TODO: remove this token skipping
		while self.current_token_is_not(TokenType::Semicolon) {
			self.next_token();
		}

		expr_stmt.into()
	}

	fn parse_statement(&mut self) -> Result<Box<Statement>> {
		match self.token_current.clone().map(|td| td.token().to_owned()) {
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
