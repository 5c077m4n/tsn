use anyhow::{bail, Result};

use super::{
	super::{
		ast::{
			Expression, ExpressionStmt, IdentifierExpr, IfExpr, InfixExpr, LetStmt, ReturnStmt,
			Statement,
		},
		token::Token,
	},
	Lexer, Parser,
};

#[test]
fn let_parsing() -> Result<()> {
	let input = r#"
    let x = 5;
    let y = 10;
    let foobar = 838383;
    "#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(parser.errors().is_empty());
	assert_eq!(program.statements.len(), 3);

	let expected_identifiers = &["x", "y", "foobar"];
	for (index, &expected) in expected_identifiers.iter().enumerate() {
		let stmt = program
			.statements
			.get(index)
			.expect("Could not get the requested statement");

		let Statement::Let(LetStmt { name, .. }) = stmt else {
			bail!("This should have been a let statement, but got {:?}", stmt);
		};
		assert_eq!(name.value, expected);
		assert_eq!(name.token.to_string(), expected);
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

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let _program = parser.parse_program()?;

	assert_eq!(
		parser.errors().len(),
		7,
		"Wrong number of errors, got {:#?}",
		parser.errors()
	);

	let expected = &[
		r#"Expected the next token to be `Equal`, but got `Some(Integer("5"))` instead"#,
		r#"Unexpected token, recieved `Some(Integer("5"))` instead of an `=` sign"#,
		"Expected the next token to be `Identifier`, but got `Some(Equal)` instead",
		"Unexpected token, recieved `Some(Equal)` instead of an identifier",
		"No parsing fn exists for the `Equal` token type",
		r#"Expected the next token to be `Identifier`, but got `Some(Integer("838383"))` instead"#,
		r#"Unexpected token, recieved `Some(Integer("838383"))` instead of an identifier"#,
	];
	assert_eq!(parser.errors(), expected);

	Ok(())
}

#[test]
fn return_parsing() -> Result<()> {
	let input = r#"
    return 5;
    return 10;
    return 993322;
    "#;

	let lexer = Lexer::new(input);
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
		assert_eq!(stmt, expected);
	}

	Ok(())
}

#[test]
fn ident_expr() -> Result<()> {
	let input = "foobar;";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(parser.errors().is_empty());
	assert_eq!(program.statements.len(), 1);

	let expr = program.statements.first().unwrap();
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
fn bool_lit_expr() -> Result<()> {
	let tests = &[("true;", true), ("false;", false)];

	for test in tests {
		let lexer = Lexer::new(test.0);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(parser.errors().is_empty());
		assert_eq!(program.statements.len(), 1);

		let expr = program.statements.first().unwrap();
		let expr = match expr {
			Statement::Expression(ExpressionStmt { expression, .. }) => expression,
			other => bail!("Should be an expression, not {:?}", other),
		};

		let expr_stmt = expr.as_ref().unwrap().as_ref();
		let ident = match expr_stmt {
			Expression::Boolean(bool_expr) => bool_expr,
			other => bail!("Should be a boolean, not {:?}", other),
		};

		assert_eq!(ident.value, test.1);
		assert_eq!(ident.token.to_string(), test.1.to_string());
	}

	Ok(())
}

#[test]
fn int_lit_expr() -> Result<()> {
	let input = "5;";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(parser.errors().is_empty());
	assert_eq!(program.statements.len(), 1);

	let expr = program.statements.first().unwrap();
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

#[test]
fn prefix_expr() -> Result<()> {
	let tests = &[("!5;", "(!5)"), ("-5;", "(-5)")];

	for test in tests {
		let lexer = Lexer::new(test.0);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(parser.errors().is_empty());
		assert_eq!(program.statements.len(), 1);

		let expr = program.statements.first().unwrap();
		let expr = match expr {
			Statement::Expression(ExpressionStmt { expression, .. }) => expression,
			other => bail!("Should not have type {:?}", other),
		};

		let expr_stmt = expr.as_ref().unwrap().as_ref();
		let prefix = match expr_stmt {
			expr @ Expression::Prefix(_) => expr,
			other => bail!("Should not have type {:?}", other),
		};
		assert_eq!(prefix.to_string(), test.1);

		let Expression::Prefix(ref prefix_expr) = **expr.as_ref().unwrap() else {
			bail!(
				"Expected a prefix expression but got: `{}`",
				**expr.as_ref().unwrap()
			)
		};

		assert_eq!(prefix_expr.op, test.1.chars().nth(1).unwrap().to_string());
		assert_eq!(prefix_expr.right.to_string(), "5");
	}

	Ok(())
}
#[test]

fn prefix_bool_expr() -> Result<()> {
	let tests = &[("!true;", true), ("!false;", false)];

	for test in tests {
		let lexer = Lexer::new(test.0);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(parser.errors().is_empty());
		assert_eq!(program.statements.len(), 1);

		let expr = program.statements.first().unwrap();
		let expr = match expr {
			Statement::Expression(ExpressionStmt { expression, .. }) => expression,
			other => bail!("Should not have type {:?}", other),
		};

		let expr_stmt = expr.as_ref().unwrap().as_ref();
		let Expression::Prefix(ref prefix_expr) = expr_stmt else {
			unreachable!("Could not extract the content of the expression")
		};

		assert_eq!(prefix_expr.op, "!");
		assert_eq!(prefix_expr.right.to_string(), test.1.to_string());
	}

	Ok(())
}

#[test]
fn infix_numbers_expr() -> Result<()> {
	let tests = &[
		("5 + 5;", 5, "+", 5),
		("5 - 5;", 5, "-", 5),
		("5 * 5;", 5, "*", 5),
		("5 / 5;", 5, "/", 5),
		("5 < 5;", 5, "<", 5),
		("5 > 5;", 5, ">", 5),
		("5 == 5;", 5, "==", 5),
		("5 != 5;", 5, "!=", 5),
	];

	for test in tests {
		let lexer = Lexer::new(test.0);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(parser.errors().is_empty());
		assert_eq!(
			program.statements.len(),
			1,
			"Wrong number of statements for test `{}`",
			test.0
		);

		let expr_stmt = program.statements.first().unwrap();
		let expr_stmt = match expr_stmt {
			Statement::Expression(ExpressionStmt { expression, .. }) => expression,
			other => bail!("Should not have type {:?}", other),
		};

		let expr_stmt = expr_stmt.as_ref().unwrap().as_ref();
		let prefix = match expr_stmt {
			expr @ Expression::Infix(_) => expr,
			other => bail!(
				"Should be of type `Expression::Infix`, not `{:?}` (in `{}`)",
				other,
				test.0
			),
		};
		assert_eq!(
			prefix.to_string(),
			format!("({} {} {})", test.1, test.2, test.3)
		);

		let Expression::Infix(ref infix_expr) = expr_stmt else {
			unreachable!("Could not extract the content of the expression")
		};

		assert_eq!(
			infix_expr.left.to_string(),
			test.1.to_string(),
			"Wrong left expression"
		);
		assert_eq!(infix_expr.op, test.2, "Wrong operator");
		assert_eq!(
			infix_expr.right.to_string(),
			test.3.to_string(),
			"Wrong right expression"
		);
	}

	Ok(())
}

#[test]
fn infix_bool_expr() -> Result<()> {
	let tests = &[
		("true == true;", true, "==", true),
		("true != false;", true, "!=", false),
		("false == false;", false, "==", false),
	];

	for test in tests {
		let lexer = Lexer::new(test.0);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(parser.errors().is_empty());
		assert_eq!(
			program.statements.len(),
			1,
			"Wrong number of statements for test `{}`",
			test.0
		);

		let expr_stmt = program.statements.first().unwrap();
		let expr_stmt = match expr_stmt {
			Statement::Expression(ExpressionStmt { expression, .. }) => expression,
			other => bail!("Should not have type {:?}", other),
		};

		let expr_stmt = expr_stmt.as_ref().unwrap().as_ref();
		let prefix = match expr_stmt {
			expr @ Expression::Infix(_) => expr,
			other => bail!(
				"Should be of type `Expression::Infix`, not `{:?}` (in `{}`)",
				other,
				test.0
			),
		};
		assert_eq!(
			prefix.to_string(),
			format!("({} {} {})", test.1, test.2, test.3)
		);

		let Expression::Infix(ref infix_expr) = expr_stmt else {
			unreachable!("Could not extract the content of the expression")
		};

		assert_eq!(
			infix_expr.left.to_string(),
			test.1.to_string(),
			"Wrong left expression"
		);
		assert_eq!(infix_expr.op, test.2, "Wrong operator");
		assert_eq!(
			infix_expr.right.to_string(),
			test.3.to_string(),
			"Wrong right expression"
		);
	}

	Ok(())
}

#[test]
fn operator_precedence_parsing() -> Result<()> {
	let tests = &[
		("-a * b;", "((-a) * b)"),
		("!-a;", "(!(-a))"),
		("a + b + c;", "((a + b) + c)"),
		("a + b - c;", "((a + b) - c)"),
		("a * b * c;", "((a * b) * c)"),
		("a * b / c;", "((a * b) / c)"),
		("a + b / c;", "(a + (b / c))"),
		("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f)"),
		("3 + 4; -5 * 5;", "(3 + 4)((-5) * 5)"),
		("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
		("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
		(
			"3 + 4 * 5 == 3 * 1 + 4 * 5;",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		),
		("true;", "true"),
		("false;", "false"),
		("3 > 5 == false;", "((3 > 5) == false)"),
		("3 < 5 == true;", "((3 < 5) == true)"),
		("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4)"),
		("(5 + 5) * 2;", "((5 + 5) * 2)"),
		("2 / (5 + 5);", "(2 / (5 + 5))"),
		("-(5 + 5);", "(-(5 + 5))"),
		("!(true == true);", "(!(true == true))"),
	];

	for test in tests {
		let lexer = Lexer::new(test.0);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(
			!program.statements.is_empty() && program.statements.len() <= 2,
			"Wrong number of statements (in `{}`)",
			test.0
		);
		assert!(
			parser.errors().is_empty(),
			"There should be no errors in the parser (in `{}`), but got: {:#?}",
			test.0,
			parser.errors()
		);
		assert_eq!(
			test.1,
			program.to_string(),
			"There is a problem in the program's string (in `{}`)",
			test.0
		);
	}

	Ok(())
}

#[test]
fn if_expression_parsing() -> Result<()> {
	let input = "if (x < y) { x }";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	let Statement::Expression(expr_stmt) = program.statements.first().unwrap() else {
		bail!(
			"The first statement should be and expression, but got a {:?}",
			program.statements.first()
		)
	};

	let Expression::If(IfExpr {
		cond, then, alt, ..
	}) = expr_stmt.expression.as_ref().unwrap().as_ref()
	else {
		bail!(
			"The first statement should be and expression, but got a {:?}",
			program.statements.first()
		)
	};

	assert_eq!(
		cond.as_ref(),
		&Expression::Infix(InfixExpr {
			token: Token::LessThan,
			left: Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("x".into()),
				value: "x".into()
			})),
			op: "<".to_string(),
			right: Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("y".into()),
				value: "y".into()
			}))
		}),
		"Wrong condition"
	);
	assert_eq!(
		then.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Identifier("x".into()),
			expression: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("x".into()),
				value: "x".to_string()
			})))
		})],
		"Wrong `then` statement"
	);
	assert!(alt.is_none(), "There should be no `else` clause");

	Ok(())
}

#[test]
fn if_else_expression_parsing() -> Result<()> {
	let input = "if (x < y) { x; } else { y }";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	let Statement::Expression(expr_stmt) = program.statements.first().unwrap() else {
		bail!(
			"The first statement should be and expression, but got a {:?}",
			program.statements.first()
		)
	};

	let Expression::If(IfExpr {
		cond, then, alt, ..
	}) = expr_stmt.expression.as_ref().unwrap().as_ref()
	else {
		bail!(
			"The first statement should be and expression, but got a {:?}",
			program.statements.first()
		)
	};

	assert_eq!(
		cond.as_ref(),
		&Expression::Infix(InfixExpr {
			token: Token::LessThan,
			left: Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("x".into()),
				value: "x".into()
			})),
			op: "<".to_string(),
			right: Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("y".into()),
				value: "y".into()
			}))
		}),
		"Wrong condition"
	);
	assert_eq!(
		then.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Identifier("x".into()),
			expression: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("x".into()),
				value: "x".to_string()
			})))
		})],
		"Wrong `then` statement"
	);

	assert!(alt.is_some(), "There should be an `else` clause");
	assert_eq!(
		alt.as_ref().unwrap().statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Identifier("y".into()),
			expression: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("y".into()),
				value: "y".to_string()
			})))
		})],
		"Wrong `then` statement"
	);

	Ok(())
}

#[test]
fn function_literal_expression_parsing() -> Result<()> {
	let input = "function (x, y) { x + y; }";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	let Statement::Expression(expr_stmt) = program.statements.first().unwrap() else {
		bail!(
			"The first statement should be and expression, but got a {:?}",
			program.statements.first()
		)
	};

	let Expression::If(IfExpr {
		cond, then, alt, ..
	}) = expr_stmt.expression.as_ref().unwrap().as_ref()
	else {
		bail!(
			"The first statement should be and expression, but got a {:?}",
			program.statements.first()
		)
	};

	assert_eq!(
		cond.as_ref(),
		&Expression::Infix(InfixExpr {
			token: Token::LessThan,
			left: Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("x".into()),
				value: "x".into()
			})),
			op: "<".to_string(),
			right: Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("y".into()),
				value: "y".into()
			}))
		}),
		"Wrong condition"
	);
	assert_eq!(
		then.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Identifier("x".into()),
			expression: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("x".into()),
				value: "x".to_string()
			})))
		})],
		"Wrong `then` statement"
	);

	assert!(alt.is_some(), "There should be an `else` clause");
	assert_eq!(
		alt.as_ref().unwrap().statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Identifier("y".into()),
			expression: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("y".into()),
				value: "y".to_string()
			})))
		})],
		"Wrong `then` statement"
	);

	Ok(())
}
