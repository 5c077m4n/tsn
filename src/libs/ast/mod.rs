use super::token::Token;

trait Node {
	fn token_literal(&self) -> String;
}

pub enum Expression {
	Identifier { token: Token, value: String },
}
impl Node for Expression {
	fn token_literal(&self) -> String {
		match self {
			Self::Identifier { token, .. } => token.to_string(),
		}
	}
}

pub enum Statement {
	Let {
		token: Token,
		name: String,
		value: Expression,
	},
}
impl Node for Statement {
	fn token_literal(&self) -> String {
		match self {
			Self::Let { token, .. } => token.to_string(),
		}
	}
}

pub struct Program {
	pub statements: Vec<Statement>,
}
impl Node for Program {
	fn token_literal(&self) -> String {
		self.statements
			.first()
			.map_or("".into(), Statement::token_literal)
	}
}
