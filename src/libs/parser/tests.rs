use anyhow::{bail, Result};

use super::{
	super::{
		ast::{LetStmt, ReturnStmt, Statement},
		token::Token,
	},
	Lexer,
	Parser,
};
use crate::libs::ast::{Expression, ExpressionStmt, IdentifierExpr};

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

#[test]
fn ident_expr() -> Result<()> {
	let input = "foobar;";

	let lexer = Lexer::new(input.into());
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert_eq!(program.statements.len(), 1);

	let expr = program.statements.get(0).unwrap().as_ref();
	let expr = match expr {
		Statement::Expression(ExpressionStmt { expression, .. }) => expression,
		other => bail!("Should not have type {:?}", other),
	};

	let expr_stmt = expr.as_ref().unwrap().as_ref();
	let ident = match expr_stmt {
		Expression::Identifier(ident_expr) => ident_expr,
		other => bail!("Should not have type {:?}", other),
	};

	assert_eq!(ident.value, "foobar");
	assert_eq!(ident.token.to_string(), "foobar");

	Ok(())
}

#[test]
fn int_lit_expr() -> Result<()> {
	let input = "5;";

	let lexer = Lexer::new(input.into());
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert_eq!(program.statements.len(), 1);

	let expr = program.statements.get(0).unwrap().as_ref();
	let expr = match expr {
		Statement::Expression(ExpressionStmt { expression, .. }) => expression,
		other => bail!("Should not have type {:?}", other),
	};

	let expr_stmt = expr.as_ref().unwrap().as_ref();
	let ident = match expr_stmt {
		Expression::Integer(int_expr) => int_expr,
		other => bail!("Should not have type {:?}", other),
	};

	assert_eq!(ident.value, 5);
	assert_eq!(ident.token.to_string(), "5");

	Ok(())
}
