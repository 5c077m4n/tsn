use std::fmt;

use super::token::Token;

trait Node: fmt::Display {
	fn token_literal(&self) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub struct IdentifierExpr {
	/// Tokne::Identifier
	pub token: Token,
	pub value: String,
}
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
	Identifier(IdentifierExpr),
}
impl Node for Expression {
	fn token_literal(&self) -> String {
		match self {
			Self::Identifier(IdentifierExpr { token, .. }) => token.to_string(),
		}
	}
}
impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Identifier(IdentifierExpr { value, .. }) => write!(f, "{}", value.to_string()),
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetStmt {
	/// Token::Let
	pub token: Token,
	pub name: Option<IdentifierExpr>,
	pub value: Option<Expression>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmt {
	pub token: Token,
	pub return_value: Option<Expression>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct ExpressionStmt {
	token: Token,
	expression: Expression,
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
			Statement::Expression(ExpressionStmt {
				token: _,
				expression,
			}) => {
				write!(f, "{}", expression.to_string())
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
