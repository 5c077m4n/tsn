use std::collections::HashMap;

use anyhow::{bail, Result};

use super::{
	ast::{
		ArrayLiteralExpr, BlockStmt, BooleanExpr, CallExpr, Expression, ExpressionStmt,
		FunctionLiteralExpr, IdentifierExpr, IfExpr, IndexExpr, InfixExpr, IntegerExpr, LetStmt,
		ObjectLiteralExpr, PrefixExpr, Program, ReturnStmt, Statement, StringExpr,
	},
	lexer::Lexer,
	token::{Token, TokenData},
};

static INFIX_TOKENS: &[Token; 12] = &[
	Token::Plus,
	Token::Minus,
	Token::Slash,
	Token::Asterisk,
	Token::DoubleEqual,
	Token::NotEqual,
	Token::LessThan,
	Token::LessThanOrEqual,
	Token::GreaterThan,
	Token::GreaterThanOrEqual,
	Token::OpenParens,
	Token::OpenSquareBraces,
];

#[derive(PartialEq, Eq, PartialOrd, Ord, Default)]
enum Precedence {
	#[default]
	Lowest,
	/// `==`
	Equals,
	/// `>`, `>=`, `<` or `<=`
	LessOrGreater,
	/// `+` or `-`
	Sum,
	/// `*` of `/`
	Product,
	/// `-X` or `!X`
	Prefix,
	/// `fnCall(X)`
	Call,
	/// objectOrArray[index]
	Index,
}
impl From<&Token> for Precedence {
	fn from(value: &Token) -> Self {
		match value {
			Token::DoubleEqual | Token::NotEqual => Self::Equals,
			Token::LessThan
			| Token::LessThanOrEqual
			| Token::GreaterThan
			| Token::GreaterThanOrEqual => Self::LessOrGreater,
			Token::Plus | Token::Minus => Self::Sum,
			Token::Asterisk | Token::Slash => Self::Product,
			Token::OpenParens => Self::Call,
			Token::OpenSquareBraces => Self::Index,
			_ => Self::default(),
		}
	}
}

pub struct Parser {
	lexer: Box<Lexer>,
	errors: Vec<String>,
	current_token_data: Option<TokenData>,
	peek_token_data: Option<TokenData>,
}
impl Parser {
	fn next_token(&mut self) {
		self.peek_token_data
			.clone_into(&mut self.current_token_data);
		self.peek_token_data = self.lexer.next();
	}

	pub fn new(lexer: Box<Lexer>) -> Self {
		let mut p = Self {
			lexer,
			errors: vec![],
			current_token_data: None,
			peek_token_data: None,
		};

		// NOTE: This is to make sure that both the `token_current` and `token_peek` are filled
		p.next_token();
		p.next_token();
		p
	}

	fn get_current_token_data(&self) -> Result<&TokenData> {
		let Some(token_data) = self.current_token_data.as_ref() else {
			bail!("The current token is empty");
		};
		Ok(token_data)
	}
	fn get_peek_token_data(&self) -> Result<&TokenData> {
		let Some(token_data) = self.peek_token_data.as_ref() else {
			bail!("The peek token is empty");
		};
		Ok(token_data)
	}

	fn current_precedence(&self) -> Result<Precedence> {
		let token = self.get_current_token_data()?.token();
		Ok(Precedence::from(token))
	}
	fn peek_precedence(&self) -> Result<Precedence> {
		let token = self.get_peek_token_data()?.token();
		Ok(Precedence::from(token))
	}

	fn parse_expression_list(&mut self, closing_token: &Token) -> Result<Vec<Expression>> {
		let mut exprs: Vec<Expression> = vec![];

		if self.peek_token_is_not(closing_token) {
			self.next_token();

			exprs.push(*self.parse_expression(Precedence::default())?);

			while self.peek_token_is(&Token::Comma) {
				self.next_token();
				self.next_token();
				exprs.push(*self.parse_expression(Precedence::default())?);
			}
			self.assert_peek(closing_token)?;
		}

		Ok(exprs)
	}

	fn infix_parse(&mut self, left: Box<Expression>) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();
		let precedence = self.current_precedence()?;
		self.next_token();

		match token {
			Token::Plus
			| Token::Minus
			| Token::Asterisk
			| Token::Slash
			| Token::DoubleEqual
			| Token::NotEqual
			| Token::GreaterThan
			| Token::GreaterThanOrEqual
			| Token::LessThan
			| Token::LessThanOrEqual => {
				let right = self.parse_expression(precedence)?;
				let infix_expr = InfixExpr {
					token: token.clone(),
					left,
					op: token.to_string(),
					right,
				};
				infix_expr.into()
			}
			Token::OpenParens => {
				self.next_token();

				let args = self.parse_expression_list(&Token::CloseParens)?;
				let call_expr = CallExpr {
					token: token.clone(),
					args,
					fn_called: left,
				};
				call_expr.into()
			}
			Token::OpenSquareBraces => {
				let index = self.parse_expression(Precedence::default())?;
				let exp = IndexExpr {
					token,
					value: left,
					index,
				};
				self.assert_peek(&Token::CloseSquareBraces)?;

				exp.into()
			}
			other => bail!("The `{:?}` token is not a valid infix operator", other),
		}
	}

	fn parse_if_expression(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();

		self.assert_peek(&Token::OpenParens)?;
		self.next_token();

		let cond = self.parse_expression(Precedence::default())?;
		self.assert_peek(&Token::CloseParens)?;

		let then = self.parse_block_statement()?;
		let mut expr = IfExpr {
			token,
			cond,
			then,
			alt: None,
		};

		if self.peek_token_is(&Token::Else) {
			self.next_token();
			expr.alt = Some(self.parse_block_statement()?);
		}

		expr.into()
	}

	fn parse_function_params(&mut self) -> Result<Vec<IdentifierExpr>> {
		self.assert_peek(&Token::OpenParens)?;

		let mut idents: Vec<IdentifierExpr> = vec![];

		if self.peek_token_is(&Token::CloseParens) {
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

		while self.peek_token_is(&Token::Comma) {
			self.next_token();
			self.next_token();

			let token = self.get_current_token_data()?.token();
			let ident = IdentifierExpr {
				token: token.clone(),
				value: token.clone().to_string(),
			};
			idents.push(ident);
		}

		self.assert_peek(&Token::CloseParens)?;

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

	fn parse_array_literal(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();
		let elements = self.parse_expression_list(&Token::CloseSquareBraces)?;

		let array = ArrayLiteralExpr { token, elements };
		array.into()
	}

	fn parse_object_literal(&mut self) -> Result<Box<Expression>> {
		let token = self.get_current_token_data()?.token().clone();
		let mut object = ObjectLiteralExpr {
			token,
			pairs: HashMap::new(),
		};

		while self.peek_token_is_not(&Token::CloseCurlyBraces) {
			self.next_token();

			let key = self.parse_expression(Precedence::default())?;
			self.assert_peek(&Token::Colon)?;

			self.next_token();
			let value = self.parse_expression(Precedence::default())?;

			object.pairs.insert(key.to_string(), *value);

			if self.peek_token_is_not(&Token::CloseCurlyBraces) {
				self.assert_peek(&Token::Comma)?;
			}
		}
		self.assert_peek(&Token::CloseCurlyBraces)?;

		object.into()
	}

	fn prefix_parse(&mut self, _precedence: Precedence) -> Result<Box<Expression>> {
		let token_data = self.get_current_token_data()?;
		let token = token_data.token();

		match token {
			Token::True | Token::False => {
				let bool_expr = BooleanExpr {
					token: token.clone(),
					value: self.current_token_is(&Token::True),
				};
				bool_expr.into()
			}
			Token::Identifier(_) => {
				let token = token.clone();

				let value = token.to_string();
				let ident_expr = IdentifierExpr { token, value };

				ident_expr.into()
			}
			Token::Integer(_) => {
				let token = token.clone();

				let value = token.to_string().parse::<usize>()?;
				let int_literal = IntegerExpr { token, value };

				int_literal.into()
			}
			Token::String(_) => {
				let token = token.clone();

				let value = token.to_string();
				let str_literal = StringExpr { token, value };

				str_literal.into()
			}
			Token::Bang | Token::Minus | Token::Plus => {
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
			Token::OpenParens => {
				self.next_token();
				let expr = self.parse_expression(Precedence::default());

				self.assert_peek(&Token::CloseParens)?;

				expr
			}
			Token::If => self.parse_if_expression(),
			Token::Function => self.parse_function_literal(),
			Token::OpenSquareBraces => self.parse_array_literal(),
			Token::OpenCurlyBraces => self.parse_object_literal(),
			other => bail!(
				"No parsing function exists for the `{:?}` token type @ {}",
				other,
				token_data.location()
			),
		}
	}

	pub fn errors(&self) -> &[String] {
		&self.errors
	}

	fn current_token_is(&self, t: &Token) -> bool {
		self.current_token_data
			.as_ref()
			.is_some_and(|token_data| token_data.token().is_of_type(t))
	}
	fn current_token_is_not(&self, t: &Token) -> bool {
		self.current_token_data
			.as_ref()
			.is_some_and(|token_data| !token_data.token().is_of_type(t))
	}
	fn peek_token_is(&self, t: &Token) -> bool {
		self.peek_token_data
			.as_ref()
			.is_some_and(|token_data| token_data.token().is_of_type(t))
	}
	fn peek_token_is_not(&self, t: &Token) -> bool {
		self.peek_token_data
			.as_ref()
			.is_some_and(|token_data| !token_data.token().is_of_type(t))
	}

	fn assert_peek(&mut self, t: &Token) -> Result<()> {
		match &self.peek_token_data {
			Some(token_data) if token_data.token().is_of_type(t) => {
				self.next_token();
				Ok(())
			}
			Some(token_data) => bail!(
				"Expected the next token to be `{:?}`, but got `{:?}` @ {} instead",
				t,
				token_data.token(),
				token_data.location()
			),
			None => bail!(
				"Expected the next token to be `{:?}`, but got nothing instead",
				t
			),
		}
	}

	fn parse_let_statement(&mut self) -> Result<Box<Statement>> {
		let let_token = self.get_current_token_data()?.token().clone();

		self.assert_peek(&Token::Identifier("_".to_string()))?;

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
		if self.current_token_is(&Token::Equal) {
			self.next_token();
			let_stmt.value = Some(self.parse_expression(Precedence::default())?);
		}
		if self.peek_token_is(&Token::Semicolon) {
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

		if self.current_token_is_not(&Token::Semicolon) {
			ret_stmt.value = Some(self.parse_expression(Precedence::default())?);
		}
		if self.peek_token_is(&Token::Semicolon) {
			self.next_token();
		}

		ret_stmt.into()
	}

	fn parse_block_statement(&mut self) -> Result<Box<BlockStmt>> {
		self.assert_peek(&Token::OpenCurlyBraces)?;

		let token = self.get_current_token_data()?.token().clone();
		let mut block = BlockStmt {
			token,
			statements: vec![],
		};
		self.next_token();

		while self.current_token_is_not(&Token::CloseCurlyBraces) {
			let stmt = self.parse_statement()?;
			block.statements.push(*stmt);

			self.next_token();
		}

		Ok(Box::new(block))
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<Expression>> {
		let mut left_expr = self.prefix_parse(Precedence::default())?;

		while {
			if let Some(token_data) = &self.current_token_data {
				!INFIX_TOKENS.contains(token_data.token())
					&& self.peek_token_is_not(&Token::Semicolon)
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
		while self.current_token_is_not(&Token::Semicolon) {
			self.next_token();
		}

		expr_stmt.into()
	}

	fn parse_statement(&mut self) -> Result<Box<Statement>> {
		match self
			.current_token_data
			.clone()
			.map(|td| td.token().to_owned())
		{
			Some(Token::Let) => self.parse_let_statement(),
			Some(Token::Return) => self.parse_return_statement(),
			None => bail!("The current token should not be empty here"),
			_ => self.parse_expression_statement(),
		}
	}

	pub fn parse_program(&mut self) -> Result<Box<Program>> {
		let mut program = Box::<Program>::default();

		while self.current_token_data.is_some() {
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
