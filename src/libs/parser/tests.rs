use anyhow::Result;

use super::{Lexer, Parser};
use crate::libs::ast::{LetStmt, Statement};

#[test]
fn test_let_parsing() -> Result<()> {
	let input = r#"
    let x = 5;
    let y = 10;
    let foobar = 838383;
    "#;

	let lexer = Lexer::new(input.into());
	let mut parser = Parser::new(Box::new(lexer));
	let program = parser.parse_program()?;

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
			other => assert!(
				false,
				"This should have been a let statement, but got {:?}",
				other
			),
		};
	}

	Ok(())
}
