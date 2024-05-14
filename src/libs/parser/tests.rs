use std::collections::HashMap;

use anyhow::{bail, Result};

use super::{
	super::{
		ast::{
			ArrayLiteralExpr, BlockStmt, BooleanExpr, Expression, ExpressionStmt,
			FunctionLiteralExpr, IdentifierExpr, IfExpr, IndexExpr, InfixExpr, IntegerExpr,
			LetStmt, ObjectLiteralExpr, PrefixExpr, ReturnStmt, Statement, StringExpr,
		},
		token::Token,
	},
	Lexer, Parser,
};

#[test]
fn let_parsing_no_errors() -> Result<()> {
	let input = r#"
    let a;
    let b = true;
    let c = false;
    let x = 5;
    let y = 10;
    let foobar = 838383;
    let foo = "bar";
    "#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"Expected no errors but got: {:#?}",
		parser.errors()
	);

	let expected_stmts = vec![
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("a".to_string()),
				value: "a".to_string(),
			},
			value: None,
		}),
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("b".to_string()),
				value: "b".to_string(),
			},
			value: Some(Box::new(Expression::Boolean(BooleanExpr {
				token: Token::True,
				value: true,
			}))),
		}),
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("c".to_string()),
				value: "c".to_string(),
			},
			value: Some(Box::new(Expression::Boolean(BooleanExpr {
				token: Token::False,
				value: false,
			}))),
		}),
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("x".to_string()),
				value: "x".to_string(),
			},
			value: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("5".to_string()),
				value: 5,
			}))),
		}),
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("y".to_string()),
				value: "y".to_string(),
			},
			value: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("10".to_string()),
				value: 10,
			}))),
		}),
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("foobar".to_string()),
				value: "foobar".to_string(),
			},
			value: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("838383".to_string()),
				value: 838383,
			}))),
		}),
		Statement::Let(LetStmt {
			token: Token::Let,
			name: IdentifierExpr {
				token: Token::Identifier("foo".to_string()),
				value: "foo".to_string(),
			},
			value: Some(Box::new(Expression::String(StringExpr {
				token: Token::String(r#""bar""#.to_string()),
				value: r#""bar""#.to_string(),
			}))),
		}),
	];
	assert_eq!(program.statements, expected_stmts);

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

	let expected = &[
		r#"Expected the next token to be `Identifier("_")`, but got `Equal` @ 3:11-3:12 instead"#,
		r#"No parsing function exists for the `Equal` token type @ 3:11-3:12"#,
		r#"Expected the next token to be `Identifier("_")`, but got `Integer("838383")` @ 4:12-5:3 instead"#,
	];
	assert_eq!(parser.errors(), expected);

	Ok(())
}

#[test]
fn return_parsing() -> Result<()> {
	let input = r#"
    return;
    return "abc";
    return true;
    return false;
    return 5;
    return 10;
    return 993322;
    "#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"Expected no errors, but got: {:#?}",
		parser.errors()
	);

	let expected_stmts = &[
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: None,
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: Some(Box::new(Expression::String(StringExpr {
				token: Token::String(r#""abc""#.to_string()),
				value: r#""abc""#.to_string(),
			}))),
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: Some(Box::new(Expression::Boolean(BooleanExpr {
				token: Token::True,
				value: true,
			}))),
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: Some(Box::new(Expression::Boolean(BooleanExpr {
				token: Token::False,
				value: false,
			}))),
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("5".to_string()),
				value: 5,
			}))),
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("10".to_string()),
				value: 10,
			}))),
		}),
		Statement::Return(ReturnStmt {
			token: Token::Return,
			value: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("993322".to_string()),
				value: 993322,
			}))),
		}),
	];
	assert_eq!(program.statements, expected_stmts);

	Ok(())
}

#[test]
fn ident_expr() -> Result<()> {
	let lexer = Lexer::new("foobar;");
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"Unexpected errors: {:?}",
		parser.errors()
	);

	assert_eq!(
		program.statements,
		&[Statement::Expression(ExpressionStmt {
			token: Token::Identifier("foobar".to_string()),
			expression: Some(Box::new(Expression::Identifier(IdentifierExpr {
				token: Token::Identifier("foobar".to_string()),
				value: "foobar".to_string()
			})))
		})]
	);

	Ok(())
}

#[test]
fn bool_lit_expr() -> Result<()> {
	let tests = &[
		("true;", true),
		("false;", false),
		("true", true),
		("false", false),
	];

	for (input, expected) in tests {
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(
			parser.errors().is_empty(),
			"Unexpected errors: {:?}",
			parser.errors()
		);

		let expected = *expected;
		assert_eq!(
			program.statements,
			&[Statement::Expression(ExpressionStmt {
				token: if expected { Token::True } else { Token::False },
				expression: Some(Box::new(Expression::Boolean(BooleanExpr {
					token: if expected { Token::True } else { Token::False },
					value: expected
				})))
			})]
		);
	}

	Ok(())
}

#[test]
fn int_lit_expr() -> Result<()> {
	let input = "5;";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"Unexpected errors: {:?}",
		parser.errors()
	);
	assert_eq!(
		program.statements,
		&[Statement::Expression(ExpressionStmt {
			token: Token::Integer("5".to_string()),
			expression: Some(Box::new(Expression::Integer(IntegerExpr {
				token: Token::Integer("5".to_string()),
				value: 5
			})))
		})]
	);

	Ok(())
}

#[test]
fn prefix_expr() -> Result<()> {
	let tests = &[("!5;", "(!5)"), ("-5;", "(-5)"), ("+5;", "(+5)")];

	for (input, expected) in tests {
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(
			parser.errors().is_empty(),
			"Unexpected errors: {:?}",
			parser.errors()
		);
		assert_eq!(program.to_string(), expected.to_string());
	}

	Ok(())
}
#[test]

fn prefix_bang_bool_expr() -> Result<()> {
	let tests = &[("!true;", true), ("!false;", false)];

	for (input, right) in tests {
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(
			parser.errors().is_empty(),
			"Unexpected errors: {:?}",
			parser.errors()
		);

		let right = *right;
		assert_eq!(
			program.statements,
			&[Statement::Expression(ExpressionStmt {
				token: Token::Bang,
				expression: Some(Box::new(Expression::Prefix(PrefixExpr {
					token: Token::Bang,
					op: "!".to_string(),
					right: Box::new(Expression::Boolean(BooleanExpr {
						token: if right { Token::True } else { Token::False },
						value: right
					}))
				})))
			})],
			"Failed to parse: {:?}",
			input
		);
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
		("5 <= 5;", 5, "<=", 5),
		("5 < 5;", 5, "<", 5),
		("5 >= 5;", 5, ">=", 5),
		("5 > 5;", 5, ">", 5),
		("5 == 5;", 5, "==", 5),
		("5 != 5;", 5, "!=", 5),
	];

	for (input, left, op, right) in tests {
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(
			parser.errors().is_empty(),
			"Unexpected errors: {:?}",
			parser.errors()
		);

		let left = *left;
		let op = op.to_string();
		let right = *right;

		assert_eq!(
			program.statements,
			&[Statement::Expression(ExpressionStmt {
				token: Token::Integer(left.to_string()),
				expression: Some(Box::new(Expression::Infix(InfixExpr {
					token: match op.as_str() {
						"+" => Token::Plus,
						"-" => Token::Minus,
						"*" => Token::Asterisk,
						"/" => Token::Slash,
						"<=" => Token::LessThanOrEqual,
						"<" => Token::LessThan,
						">=" => Token::GreaterThanOrEqual,
						">" => Token::GreaterThan,
						"==" => Token::DoubleEqual,
						"!=" => Token::NotEqual,
						other => bail!("Unsupported operator {:?}", other),
					},
					left: Box::new(Expression::Integer(IntegerExpr {
						token: Token::Integer(left.to_string()),
						value: left
					})),
					op: op.to_string(),
					right: Box::new(Expression::Integer(IntegerExpr {
						token: Token::Integer(right.to_string()),
						value: right
					}))
				})))
			})]
		);
	}

	Ok(())
}

#[test]
fn infix_bool_expr() -> Result<()> {
	let tests = &[
		("true == true;", true, "==", true),
		("true != false;", true, "!=", false),
		("false != true;", false, "!=", true),
		("false == false;", false, "==", false),
	];

	for (input, left, op, right) in tests {
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		let left = *left;
		let op = op.to_string();
		let right = *right;

		assert!(
			parser.errors().is_empty(),
			"Unexpected errors: {:?}",
			parser.errors()
		);
		assert_eq!(
			program.statements,
			&[Statement::Expression(ExpressionStmt {
				token: if left { Token::True } else { Token::False },
				expression: Some(Box::new(Expression::Infix(InfixExpr {
					token: if op == "==" {
						Token::DoubleEqual
					} else {
						Token::NotEqual
					},
					left: Box::new(Expression::Boolean(BooleanExpr {
						token: if left { Token::True } else { Token::False },
						value: left
					})),
					op,
					right: Box::new(Expression::Boolean(BooleanExpr {
						token: if right { Token::True } else { Token::False },
						value: right
					}))
				})))
			})],
			"Failed parsing {:?}",
			input
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

	for (input, expected) in tests {
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(Box::new(lexer));
		let program = parser.parse_program()?;

		assert!(
			!program.statements.is_empty() && program.statements.len() <= 2,
			"Wrong number of statements (in `{}`)",
			input
		);
		assert!(
			parser.errors().is_empty(),
			"There should be no errors in the parser (in `{}`), but got: {:#?}",
			input,
			parser.errors()
		);
		assert_eq!(
			expected.to_string(),
			program.to_string(),
			"There is a problem in the program's string (in `{}`)",
			input
		);
	}

	Ok(())
}

#[test]
fn if_without_else_expression_parsing() -> Result<()> {
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
	let input = "if (x < y) { x; } else { y; }";

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
		"Wrong `else` statement"
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

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Function,
			expression: Some(Box::new(Expression::FunctionLiteral(FunctionLiteralExpr {
				token: Token::Function,
				params: vec![
					IdentifierExpr {
						token: Token::Identifier("x".to_string()),
						value: "x".to_string()
					},
					IdentifierExpr {
						token: Token::Identifier("y".to_string()),
						value: "y".to_string()
					}
				],
				body: Box::new(BlockStmt {
					token: Token::OpenCurlyBraces,
					statements: vec![Statement::Expression(ExpressionStmt {
						token: Token::Identifier("x".to_string()),
						expression: Some(Box::new(Expression::Infix(InfixExpr {
							token: Token::Plus,
							left: Box::new(Expression::Identifier(IdentifierExpr {
								token: Token::Identifier("x".to_string()),
								value: "x".to_string()
							})),
							op: "+".to_string(),
							right: Box::new(Expression::Identifier(IdentifierExpr {
								token: Token::Identifier("y".to_string()),
								value: "y".to_string()
							}))
						})))
					})]
				})
			})))
		})]
	);

	Ok(())
}

#[test]
fn empty_array_literal_expression_parsing() -> Result<()> {
	let input = "[]";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::OpenSquareBraces,
			expression: Some(Box::new(Expression::ArrayLiteral(ArrayLiteralExpr {
				token: Token::OpenSquareBraces,
				elements: vec![]
			})))
		})]
	);

	Ok(())
}
#[test]
fn array_literal_expression_parsing() -> Result<()> {
	let input = r#"[1, 2, 3, "some string", true]"#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::OpenSquareBraces,
			expression: Some(Box::new(Expression::ArrayLiteral(ArrayLiteralExpr {
				token: Token::OpenSquareBraces,
				elements: vec![
					Expression::Integer(IntegerExpr {
						token: Token::Integer("1".to_string()),
						value: 1
					}),
					Expression::Integer(IntegerExpr {
						token: Token::Integer("2".to_string()),
						value: 2
					}),
					Expression::Integer(IntegerExpr {
						token: Token::Integer("3".to_string()),
						value: 3
					}),
					Expression::String(StringExpr {
						token: Token::String("\"some string\"".to_string()),
						value: "\"some string\"".to_string(),
					}),
					Expression::Boolean(BooleanExpr {
						token: Token::True,
						value: true,
					})
				]
			})))
		})]
	);

	Ok(())
}

#[test]
fn array_literal_expression_error_parsing() -> Result<()> {
	let input = r#"[1, 2, 3,, "some string", true]"#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let _program = parser.parse_program()?;

	assert_eq!(
		parser.errors(),
		&["No parsing function exists for the `Comma` token type @ 1:10-1:11".to_string()]
	);

	Ok(())
}

#[test]
fn empty_object_literal_expression_parsing() -> Result<()> {
	let input = "{}";

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::OpenCurlyBraces,
			expression: Some(Box::new(Expression::ObjectLiteral(ObjectLiteralExpr {
				token: Token::OpenCurlyBraces,
				pairs: HashMap::new()
			})))
		})]
	);

	Ok(())
}

#[test]
fn object_literal_expression_parsing() -> Result<()> {
	let input = r#"{ "one": 1, "two": true, "sum": 1 / 2 + 3 }"#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	let pairs: HashMap<String, Expression> = [
		(
			r#""one""#.to_string(),
			Expression::Integer(IntegerExpr {
				token: Token::Integer("1".to_string()),
				value: 1,
			}),
		),
		(
			r#""two""#.to_string(),
			Expression::Boolean(BooleanExpr {
				token: Token::True,
				value: true,
			}),
		),
		(
			r#""sum""#.to_string(),
			Expression::Infix(InfixExpr {
				token: Token::Plus,
				left: Box::new(Expression::Infix(InfixExpr {
					token: Token::Slash,
					left: Box::new(Expression::Integer(IntegerExpr {
						token: Token::Integer("1".to_string()),
						value: 1,
					})),
					op: "/".to_string(),
					right: Box::new(Expression::Integer(IntegerExpr {
						token: Token::Integer("2".to_string()),
						value: 2,
					})),
				})),
				op: "+".to_string(),
				right: Box::new(Expression::Integer(IntegerExpr {
					token: Token::Integer("3".to_string()),
					value: 3,
				})),
			}),
		),
	]
	.into();

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::OpenCurlyBraces,
			expression: Some(Box::new(Expression::ObjectLiteral(ObjectLiteralExpr {
				token: Token::OpenCurlyBraces,
				pairs
			})))
		})]
	);

	Ok(())
}

#[test]
fn some_ident_index_expression_parsing() -> Result<()> {
	let input = r#"myObject["one"]"#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::Identifier("myObject".to_string()),
			expression: Some(Box::new(Expression::Index(IndexExpr {
				token: Token::OpenSquareBraces,
				value: Box::new(Expression::Identifier(IdentifierExpr {
					token: Token::Identifier("myObject".to_string()),
					value: "myObject".to_string(),
				})),
				index: Box::new(Expression::String(StringExpr {
					token: Token::String("one".to_string()),
					value: "one".to_string()
				})),
			})))
		})]
	);

	Ok(())
}

#[test]
fn object_literal_index_expression_parsing() -> Result<()> {
	let input = r#"{ "one": 1, "two": true, "sum": 1 / 2 + 3 }["one"]"#;

	let lexer = Lexer::new(input);
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

	assert!(
		parser.errors().is_empty(),
		"There should be no errors in the parser, but got: {:#?}",
		parser.errors()
	);

	let pairs: HashMap<String, Expression> = [
		(
			r#""one""#.to_string(),
			Expression::Integer(IntegerExpr {
				token: Token::Integer("1".to_string()),
				value: 1,
			}),
		),
		(
			r#""two""#.to_string(),
			Expression::Boolean(BooleanExpr {
				token: Token::True,
				value: true,
			}),
		),
		(
			r#""sum""#.to_string(),
			Expression::Infix(InfixExpr {
				token: Token::Plus,
				left: Box::new(Expression::Infix(InfixExpr {
					token: Token::Slash,
					left: Box::new(Expression::Integer(IntegerExpr {
						token: Token::Integer("1".to_string()),
						value: 1,
					})),
					op: "/".to_string(),
					right: Box::new(Expression::Integer(IntegerExpr {
						token: Token::Integer("2".to_string()),
						value: 2,
					})),
				})),
				op: "+".to_string(),
				right: Box::new(Expression::Integer(IntegerExpr {
					token: Token::Integer("3".to_string()),
					value: 3,
				})),
			}),
		),
	]
	.into();

	assert_eq!(
		program.statements,
		vec![Statement::Expression(ExpressionStmt {
			token: Token::OpenCurlyBraces,
			expression: Some(Box::new(Expression::Index(IndexExpr {
				token: Token::OpenSquareBraces,
				value: Box::new(Expression::ObjectLiteral(ObjectLiteralExpr {
					token: Token::OpenCurlyBraces,
					pairs
				})),
				index: Box::new(Expression::String(StringExpr {
					token: Token::String("one".to_string()),
					value: "one".to_string()
				})),
			})))
		})]
	);

	Ok(())
}
