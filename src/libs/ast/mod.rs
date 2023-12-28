use super::token::Token;

trait Node {
	fn token_literal(&self) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub struct IdentifierExpr {
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

#[derive(Debug, PartialEq, Eq)]
pub struct LetStmt {
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
pub enum Statement {
	Let(LetStmt),
	Return(ReturnStmt),
}
impl Node for Statement {
	fn token_literal(&self) -> String {
		match self {
			Self::Let(LetStmt { token, .. }) => token.to_string(),
			Statement::Return(ReturnStmt { token, .. }) => token.to_string(),
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
			.map_or("".into(), |stmt| stmt.token_literal())
	}
}
