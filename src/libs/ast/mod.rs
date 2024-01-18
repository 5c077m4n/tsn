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
	pub right: Box<Expression>,
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
	pub right: Box<Expression>,
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
pub struct IfExpr {
	/// `Token::If`
	pub token: Token,
	pub cond: Box<Expression>,
	pub then: Box<BlockStmt>,
	pub alt: Option<Box<BlockStmt>>,
}
impl Into<Result<Box<Expression>>> for IfExpr {
	fn into(self) -> Result<Box<Expression>> {
		Ok(Box::new(Expression::If(self)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
	Identifier(IdentifierExpr),
	Integer(IntegerExpr),
	Prefix(PrefixExpr),
	Infix(InfixExpr),
	Boolean(BooleanExpr),
	If(IfExpr),
}
impl Node for Expression {
	fn token_literal(&self) -> String {
		match self {
			Self::Identifier(IdentifierExpr { token, .. }) => token.to_string(),
			Self::Integer(IntegerExpr { token, .. }) => token.to_string(),
			Self::Prefix(PrefixExpr { token, .. }) => token.to_string(),
			Self::Infix(InfixExpr { token, .. }) => token.to_string(),
			Self::Boolean(BooleanExpr { token, .. }) => token.to_string(),
			Self::If(IfExpr { token, .. }) => token.to_string(),
		}
	}
}
impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Identifier(IdentifierExpr { value, .. }) => write!(f, "{}", value),
			Self::Integer(IntegerExpr { value, .. }) => write!(f, "{}", value),
			Self::Prefix(PrefixExpr { op, right, .. }) => {
				write!(f, "({}{})", op, right.to_string())
			}
			Self::Infix(InfixExpr {
				left, op, right, ..
			}) => {
				write!(f, "({} {} {})", left.to_string(), op, right.to_string())
			}
			Self::Boolean(BooleanExpr { value, .. }) => write!(f, "{}", value),
			Self::If(IfExpr {
				cond, then, alt, ..
			}) => {
				write!(f, "if ({}) {{ {} }}", cond.to_string(), then.to_string())?;
				if let Some(alt) = alt {
					write!(f, " else {{ {} }}", alt.to_string())?;
				}
				Ok(())
			}
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetStmt {
	/// Token::Let
	pub token: Token,
	pub name: IdentifierExpr,
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
pub struct BlockStmt {
	pub token: Token,
	pub statements: Vec<Statement>,
}
impl Into<Result<Box<Statement>>> for BlockStmt {
	fn into(self) -> Result<Box<Statement>> {
		Ok(Box::new(Statement::Block(self)))
	}
}
impl fmt::Display for BlockStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt.to_string())?;
		}
		Ok(())
	}
}
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
	Let(LetStmt),
	Return(ReturnStmt),
	Expression(ExpressionStmt),
	Block(BlockStmt),
}
impl Node for Statement {
	fn token_literal(&self) -> String {
		match self {
			Self::Let(LetStmt { token, .. }) => token.to_string(),
			Self::Return(ReturnStmt { token, .. }) => token.to_string(),
			Self::Expression(ExpressionStmt { token, .. }) => token.to_string(),
			Self::Block(BlockStmt { token, .. }) => token.to_string(),
		}
	}
}
impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Let(LetStmt { token, name, value }) => {
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
			Self::Block(block_stmt) => write!(f, "{}", block_stmt),
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
