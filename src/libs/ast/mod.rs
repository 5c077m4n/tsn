use std::fmt;

use anyhow::Result;

use super::token::Token;

trait Node: fmt::Display {
	fn token_literal(&self) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub struct IdentifierExpr {
	/// Token::Identifier
	pub token: Token,
	pub value: String,
}
impl Into<Result<Box<Expression>>> for IdentifierExpr {
	fn into(self) -> Result<Box<Expression>> {
		Ok(Box::new(Expression::Identifier(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct IntegerExpr {
	pub token: Token,
	pub value: usize,
}
impl Into<Result<Box<Expression>>> for IntegerExpr {
	fn into(self) -> Result<Box<Expression>> {
		Ok(Box::new(Expression::Integer(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct PrefixExpr {
	/// `Token::Bang` or `Token::Minus`
	pub token: Token,
	pub op: String,
	pub right: Option<Box<Expression>>,
}
impl Into<Result<Box<Expression>>> for PrefixExpr {
	fn into(self) -> Result<Box<Expression>> {
		Ok(Box::new(Expression::Prefix(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct InfixExpr {
	/// `Token::Bang` or `Token::Minus`
	pub token: Token,
	pub left: Box<Expression>,
	pub op: String,
	pub right: Option<Box<Expression>>,
}
impl Into<Result<Box<Expression>>> for InfixExpr {
	fn into(self) -> Result<Box<Expression>> {
		Ok(Box::new(Expression::Infix(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct BooleanExpr {
	/// `Token::Bang` or `Token::Minus`
	pub token: Token,
	pub value: bool,
}
impl Into<Result<Box<Expression>>> for BooleanExpr {
	fn into(self) -> Result<Box<Expression>> {
		Ok(Box::new(Expression::Boolean(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
	Identifier(IdentifierExpr),
	Integer(IntegerExpr),
	Prefix(PrefixExpr),
	Infix(InfixExpr),
	Boolean(BooleanExpr),
}
impl Node for Expression {
	fn token_literal(&self) -> String {
		match self {
			Self::Identifier(IdentifierExpr { token, .. }) => token.to_string(),
			Self::Integer(IntegerExpr { token, .. }) => token.to_string(),
			Self::Prefix(PrefixExpr { token, .. }) => token.to_string(),
			Self::Infix(InfixExpr { token, .. }) => token.to_string(),
			Self::Boolean(BooleanExpr { token, .. }) => token.to_string(),
		}
	}
}
impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Identifier(IdentifierExpr { value, .. }) => write!(f, "{}", value),
			Self::Integer(IntegerExpr { value, .. }) => write!(f, "{}", value),
			Self::Prefix(PrefixExpr { op, right, .. }) => {
				write!(
					f,
					"({}{})",
					op,
					right.as_ref().map_or("".into(), |r| r.to_string())
				)
			}
			Self::Infix(InfixExpr {
				left, op, right, ..
			}) => {
				write!(
					f,
					"({} {} {})",
					left.to_string(),
					op,
					right.as_ref().map_or("".into(), |r| r.to_string())
				)
			}
			Self::Boolean(BooleanExpr { value, .. }) => write!(f, "{}", value),
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetStmt {
	/// Token::Let
	pub token: Token,
	pub name: Option<IdentifierExpr>,
	pub value: Option<Box<Expression>>,
}
impl Into<Result<Box<Statement>>> for LetStmt {
	fn into(self) -> Result<Box<Statement>> {
		Ok(Box::new(Statement::Let(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmt {
	pub token: Token,
	pub return_value: Option<Box<Expression>>,
}
impl Into<Result<Box<Statement>>> for ReturnStmt {
	fn into(self) -> Result<Box<Statement>> {
		Ok(Box::new(Statement::Return(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct ExpressionStmt {
	pub token: Token,
	pub expression: Option<Box<Expression>>,
}
impl Into<Result<Box<Statement>>> for ExpressionStmt {
	fn into(self) -> Result<Box<Statement>> {
		Ok(Box::new(Statement::Expression(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
	Let(LetStmt),
	Return(ReturnStmt),
	Expression(ExpressionStmt),
}
impl Node for Statement {
	fn token_literal(&self) -> String {
		match self {
			Self::Let(LetStmt { token, .. }) => token.to_string(),
			Self::Return(ReturnStmt { token, .. }) => token.to_string(),
			Self::Expression(ExpressionStmt { token, .. }) => token.to_string(),
		}
	}
}
impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Let(LetStmt { token, name, value }) => {
				let name = name
					.as_ref()
					.expect("The name of the variable definition should exist by now");
				write!(f, "{} {}", token.to_string(), name.token.to_string())?;

				if let Some(v) = value {
					write!(f, " = {}", v.to_string())?;
				}
				write!(f, ";")
			}
			Statement::Return(ReturnStmt {
				token,
				return_value,
			}) => {
				write!(f, "{} ", token.to_string())?;
				if let Some(ret_val) = return_value {
					write!(f, " = {}", ret_val.to_string())?;
				}
				write!(f, ";")
			}
			Statement::Expression(ExpressionStmt { expression, .. }) => {
				if let Some(expr) = expression {
					write!(f, "{}", expr.to_string())
				} else {
					Err(fmt::Error)
				}
			}
		}
	}
}

#[derive(Default)]
pub struct Program {
	pub statements: Vec<Box<Statement>>,
}
impl Node for Program {
	fn token_literal(&self) -> String {
		self.statements
			.first()
			.map_or("".into(), |stmt| stmt.to_string())
	}
}
impl fmt::Display for Program {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for stmt in self.statements.as_slice() {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}

#[cfg(test)]
mod tests;
