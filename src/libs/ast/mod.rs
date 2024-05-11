use std::collections::HashMap;
use std::fmt;

use anyhow::Result;

use super::token::Token;

#[allow(dead_code)]
trait Node: fmt::Display {
	fn token_literal(&self) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub struct IdentifierExpr {
	/// Token::Identifier
	pub token: Token,
	pub value: String,
}
impl From<IdentifierExpr> for Result<Box<Expression>> {
	fn from(value: IdentifierExpr) -> Self {
		Ok(Box::new(Expression::Identifier(value)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct IntegerExpr {
	pub token: Token,
	pub value: usize,
}
impl From<IntegerExpr> for Result<Box<Expression>> {
	fn from(val: IntegerExpr) -> Self {
		Ok(Box::new(Expression::Integer(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct StringExpr {
	pub token: Token,
	pub value: String,
}
impl From<StringExpr> for Result<Box<Expression>> {
	fn from(val: StringExpr) -> Self {
		Ok(Box::new(Expression::String(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct PrefixExpr {
	/// `Token::Bang` or `Token::Minus`
	pub token: Token,
	pub op: String,
	pub right: Box<Expression>,
}
impl From<PrefixExpr> for Result<Box<Expression>> {
	fn from(val: PrefixExpr) -> Self {
		Ok(Box::new(Expression::Prefix(val)))
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
impl From<InfixExpr> for Result<Box<Expression>> {
	fn from(val: InfixExpr) -> Self {
		Ok(Box::new(Expression::Infix(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct BooleanExpr {
	/// `Token::True` or `Token::False`
	pub token: Token,
	pub value: bool,
}
impl From<BooleanExpr> for Result<Box<Expression>> {
	fn from(val: BooleanExpr) -> Self {
		Ok(Box::new(Expression::Boolean(val)))
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
impl From<IfExpr> for Result<Box<Expression>> {
	fn from(val: IfExpr) -> Self {
		Ok(Box::new(Expression::If(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionLiteralExpr {
	/// `Token::Function`
	pub token: Token,
	pub params: Vec<IdentifierExpr>,
	pub body: Box<BlockStmt>,
}
impl From<FunctionLiteralExpr> for Result<Box<Expression>> {
	fn from(val: FunctionLiteralExpr) -> Self {
		Ok(Box::new(Expression::FunctionLiteral(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct ArrayLiteralExpr {
	/// `Token::OpenSquareBraces`
	pub token: Token,
	pub elements: Vec<Expression>,
}
impl From<ArrayLiteralExpr> for Result<Box<Expression>> {
	fn from(val: ArrayLiteralExpr) -> Self {
		Ok(Box::new(Expression::ArrayLiteral(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct ObjectLiteralExpr {
	/// `Token::OpenCurlyBraces`
	pub token: Token,
	pub pairs: HashMap<String, Expression>,
}
impl From<ObjectLiteralExpr> for Result<Box<Expression>> {
	fn from(val: ObjectLiteralExpr) -> Self {
		Ok(Box::new(Expression::ObjectLiteral(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
	Identifier(IdentifierExpr),
	Integer(IntegerExpr),
	String(StringExpr),
	Prefix(PrefixExpr),
	Infix(InfixExpr),
	Boolean(BooleanExpr),
	If(IfExpr),
	FunctionLiteral(FunctionLiteralExpr),
	ArrayLiteral(ArrayLiteralExpr),
	ObjectLiteral(ObjectLiteralExpr),
}
impl Node for Expression {
	fn token_literal(&self) -> String {
		match self {
			Self::Identifier(IdentifierExpr { token, .. }) => token.to_string(),
			Self::Integer(IntegerExpr { token, .. }) => token.to_string(),
			Expression::String(StringExpr { token, .. }) => token.to_string(),
			Self::Prefix(PrefixExpr { token, .. }) => token.to_string(),
			Self::Infix(InfixExpr { token, .. }) => token.to_string(),
			Self::Boolean(BooleanExpr { token, .. }) => token.to_string(),
			Self::If(IfExpr { token, .. }) => token.to_string(),
			Self::FunctionLiteral(FunctionLiteralExpr { token, .. }) => token.to_string(),
			Self::ArrayLiteral(ArrayLiteralExpr { token, .. }) => token.to_string(),
			Self::ObjectLiteral(ObjectLiteralExpr { token, .. }) => token.to_string(),
		}
	}
}
impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Identifier(IdentifierExpr { value, .. }) => write!(f, "{}", value),
			Self::Integer(IntegerExpr { value, .. }) => write!(f, "{}", value),
			Self::String(StringExpr { value, .. }) => write!(f, "{}", value),
			Self::Prefix(PrefixExpr { op, right, .. }) => {
				write!(f, "({}{})", op, right)
			}
			Self::Infix(InfixExpr {
				left, op, right, ..
			}) => {
				write!(f, "({} {} {})", left, op, right)
			}
			Self::Boolean(BooleanExpr { value, .. }) => write!(f, "{}", value),
			Self::If(IfExpr {
				cond, then, alt, ..
			}) => {
				write!(f, "if ({}) {{ {} }}", cond, then)?;
				if let Some(alt) = alt {
					write!(f, " else {{ {} }}", alt)?;
				}
				Ok(())
			}
			Self::FunctionLiteral(FunctionLiteralExpr {
				token,
				params,
				body,
			}) => {
				let params = params
					.iter()
					.map(|p| p.value.clone())
					.collect::<Vec<String>>()
					.join(", ");
				write!(f, "{} ({}) {}", token, params, body)
			}
			Self::ArrayLiteral(ArrayLiteralExpr { token: _, elements }) => {
				write!(f, "[")?;
				for el in elements {
					write!(f, "{}, ", el)?;
				}
				write!(f, "]")
			}
			Self::ObjectLiteral(ObjectLiteralExpr { token: _, pairs }) => {
				write!(f, "{{ ")?;
				for (key, value) in pairs {
					write!(f, "{}: {}, ", key, value)?;
				}
				write!(f, " }}")
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
impl From<LetStmt> for Result<Box<Statement>> {
	fn from(val: LetStmt) -> Self {
		Ok(Box::new(Statement::Let(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmt {
	pub token: Token,
	pub value: Option<Box<Expression>>,
}
impl From<ReturnStmt> for Result<Box<Statement>> {
	fn from(val: ReturnStmt) -> Self {
		Ok(Box::new(Statement::Return(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct ExpressionStmt {
	pub token: Token,
	pub expression: Option<Box<Expression>>,
}
impl From<ExpressionStmt> for Result<Box<Statement>> {
	fn from(val: ExpressionStmt) -> Self {
		Ok(Box::new(Statement::Expression(val)))
	}
}
#[derive(Debug, PartialEq, Eq)]
pub struct BlockStmt {
	pub token: Token,
	pub statements: Vec<Statement>,
}
impl From<BlockStmt> for Result<Box<Statement>> {
	fn from(val: BlockStmt) -> Self {
		Ok(Box::new(Statement::Block(val)))
	}
}
impl fmt::Display for BlockStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt)?;
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
				write!(f, "{} {}", token, name.token)?;

				if let Some(v) = value {
					write!(f, " = {}", v)?;
				}
				write!(f, ";")
			}
			Statement::Return(ReturnStmt {
				token,
				value: return_value,
			}) => {
				write!(f, "{} ", token)?;
				if let Some(ret_val) = return_value {
					write!(f, " = {}", ret_val)?;
				}
				write!(f, ";")
			}
			Statement::Expression(ExpressionStmt { expression, .. }) => {
				if let Some(expr) = expression {
					write!(f, "{}", expr)
				} else {
					Err(fmt::Error)
				}
			}
			Self::Block(block_stmt) => write!(f, "{}", block_stmt),
		}
	}
}

#[derive(Debug, Default)]
pub struct Program {
	pub statements: Vec<Statement>,
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
