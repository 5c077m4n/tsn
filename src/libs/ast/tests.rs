use anyhow::Result;

use super::{
	super::ast::{Expression, IdentifierExpr, LetStmt, Statement},
	Program, Token,
};

#[test]
fn let_stmt_var_assign() -> Result<()> {
	let program = &Program {
		statements: vec![Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier(b"myVar".into()),
				value: "myVar".to_string(),
			},
			value: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier(b"anotherVar".into()),
				value: "anotherVar".to_string(),
			}))),
		})],
	};
	assert_eq!(program.to_string(), "let myVar = anotherVar;".to_string());

	Ok(())
}
