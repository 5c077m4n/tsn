use anyhow::Result;

use super::{
	super::{
		ast::{Expression, LetStmt, ReturnStmt, Statement},
		token::Token,
	},
	Lexer,
	Parser,
};
use crate::libs::ast::IdentifierExpr;

#[test]
fn let_parsing() -> Result<()> {
	let input = r#"
    let x = 5;
    let y = 10;
    let foobar = 838383;
    "#;

	let lexer = Lexer::new(input.into());
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(parser.errors().is_empty());
	assert_eq!(program.statements.len(), 3);

	let expected_identifiers = &["x", "y", "foobar"];
	for (index, &expected) in expected_identifiers.iter().enumerate() {
		let stmt = program.statements.get(index).unwrap();
		let stmt = stmt.as_ref();

		match stmt {
			Statement::Let(LetStmt { name, .. }) => {
				assert!(name.is_some());
				assert_eq!(name.as_ref().unwrap().value, expected);
				assert_eq!(name.as_ref().unwrap().token.to_string(), expected);
			}
			#[allow(unreachable_patterns)]
			other => assert!(
				false,
				"This should have been a let statement, but got {:?}",
				other
			),
		};
	}

	Ok(())
}

#[test]
fn let_parsing_errors() -> Result<()> {
	let input = r#"
    let x 5;
    let = 10;
    let 838383;
    "#;

	let lexer = Lexer::new(input.into());
	let mut parser = Parser::new(Box::new(lexer));
	let _program = parser.parse_program()?;

	assert_eq!(parser.errors().len(), 3);
	assert_eq!(
		parser.errors().get(0),
		Some(
			&"Expected the next token to be `Eq`, but got `Some(Integer([53]))` instead"
				.to_string()
		)
	);
	assert_eq!(
		parser.errors().get(1),
		Some(&"Expected the next token to be `Identifier`, but got `Some(Eq)` instead".to_string())
	);
	assert_eq!(
		parser.errors().get(2),
		Some(&"Expected the next token to be `Identifier`, but got `Some(Integer([56, 51, 56, 51, 56, 51]))` instead".to_string())
	);

	Ok(())
}

#[test]
fn return_parsing() -> Result<()> {
	let input = r#"
    return 5;
    return 10;
    return 993322;
    "#;

	let lexer = Lexer::new(input.into());
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(parser.errors().is_empty());
	assert_eq!(program.statements.len(), 3);

	let expected_stmts = &[
		Statement::Return(ReturnStmt {
			token: Token::Return,
			return_value: None,
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			return_value: None,
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			return_value: None,
		}),
	];
	for (index, stmt) in program.statements.iter().enumerate() {
		let expected = expected_stmts.get(index).unwrap();
		assert_eq!(stmt.as_ref(), expected);
	}

	Ok(())
}
