use super::{
	super::token::{Token, TokenData},
	Lexer,
};

#[test]
fn simple_token_read() {
	let input = "=+(){},; \t";
	let expected = vec![
		TokenData::new(Token::Equal, 0, input.as_bytes()),
		TokenData::new(Token::Plus, 1, input.as_bytes()),
		TokenData::new(Token::OpenParens, 2, input.as_bytes()),
		TokenData::new(Token::CloseParens, 3, input.as_bytes()),
		TokenData::new(Token::OpenCurlyBraces, 4, input.as_bytes()),
		TokenData::new(Token::CloseCurlyBraces, 5, input.as_bytes()),
		TokenData::new(Token::Comma, 6, input.as_bytes()),
		TokenData::new(Token::Semicolon, 7, input.as_bytes()),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn let_stmt_token_read() {
	let input = "let param = 10;";
	let expected = vec![
		TokenData::new(Token::Let, 0, input.as_bytes()),
		TokenData::new(Token::Identifier("param".into()), 4, input.as_bytes()),
		TokenData::new(Token::Equal, 10, input.as_bytes()),
		TokenData::new(Token::Integer("10".into()), 12, input.as_bytes()),
		TokenData::new(Token::Semicolon, 14, input.as_bytes()),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results, "input `{}`", input);
}

#[test]
fn bang_sign_token_read() {
	let input = r#"!"#;
	let expected = vec![TokenData::new(Token::Bang, 0, input.as_bytes())];

	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn math_signs_token_read() {
	let input = r#"
    !-/*5;
    5 < 10 > 5;
    "#;
	let expected = vec![
		TokenData::new(Token::Bang, 5, input.as_bytes()),
		TokenData::new(Token::Minus, 6, input.as_bytes()),
		TokenData::new(Token::Slash, 7, input.as_bytes()),
		TokenData::new(Token::Asterisk, 8, input.as_bytes()),
		TokenData::new(Token::Integer("5".into()), 9, input.as_bytes()),
		TokenData::new(Token::Semicolon, 10, input.as_bytes()),
		TokenData::new(Token::Integer("5".into()), 16, input.as_bytes()),
		TokenData::new(Token::LessThan, 18, input.as_bytes()),
		TokenData::new(Token::Integer("10".into()), 20, input.as_bytes()),
		TokenData::new(Token::GreaterThan, 23, input.as_bytes()),
		TokenData::new(Token::Integer("5".into()), 25, input.as_bytes()),
		TokenData::new(Token::Semicolon, 26, input.as_bytes()),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn functional_token_read() {
	let input = r#"let five = 5;
    let ten = 10;

    let add = function(x, y) {
      x + y;
    };

    let result = add(five, ten);
    "#;
	let expected = vec![
		TokenData::new(Token::Let, 0, input.as_bytes()),
		TokenData::new(Token::Identifier("five".into()), 4, input.as_bytes()),
		TokenData::new(Token::Equal, 9, input.as_bytes()),
		TokenData::new(Token::Integer("5".into()), 11, input.as_bytes()),
		TokenData::new(Token::Semicolon, 12, input.as_bytes()),
		TokenData::new(Token::Let, 18, input.as_bytes()),
		TokenData::new(Token::Identifier("ten".into()), 22, input.as_bytes()),
		TokenData::new(Token::Equal, 26, input.as_bytes()),
		TokenData::new(Token::Integer("10".into()), 28, input.as_bytes()),
		TokenData::new(Token::Semicolon, 30, input.as_bytes()),
		TokenData::new(Token::Let, 37, input.as_bytes()),
		TokenData::new(Token::Identifier("add".into()), 41, input.as_bytes()),
		TokenData::new(Token::Equal, 45, input.as_bytes()),
		TokenData::new(Token::Function, 47, input.as_bytes()),
		TokenData::new(Token::OpenParens, 55, input.as_bytes()),
		TokenData::new(Token::Identifier("x".into()), 56, input.as_bytes()),
		TokenData::new(Token::Comma, 57, input.as_bytes()),
		TokenData::new(Token::Identifier("y".into()), 59, input.as_bytes()),
		TokenData::new(Token::CloseParens, 60, input.as_bytes()),
		TokenData::new(Token::OpenCurlyBraces, 62, input.as_bytes()),
		TokenData::new(Token::Identifier("x".into()), 70, input.as_bytes()),
		TokenData::new(Token::Plus, 72, input.as_bytes()),
		TokenData::new(Token::Identifier("y".into()), 74, input.as_bytes()),
		TokenData::new(Token::Semicolon, 75, input.as_bytes()),
		TokenData::new(Token::CloseCurlyBraces, 81, input.as_bytes()),
		TokenData::new(Token::Semicolon, 82, input.as_bytes()),
		TokenData::new(Token::Let, 89, input.as_bytes()),
		TokenData::new(Token::Identifier("result".into()), 93, input.as_bytes()),
		TokenData::new(Token::Equal, 100, input.as_bytes()),
		TokenData::new(Token::Identifier("add".into()), 102, input.as_bytes()),
		TokenData::new(Token::OpenParens, 105, input.as_bytes()),
		TokenData::new(Token::Identifier("five".into()), 106, input.as_bytes()),
		TokenData::new(Token::Comma, 110, input.as_bytes()),
		TokenData::new(Token::Identifier("ten".into()), 112, input.as_bytes()),
		TokenData::new(Token::CloseParens, 115, input.as_bytes()),
		TokenData::new(Token::Semicolon, 116, input.as_bytes()),
	];

	let lexer = Lexer::new(input);
	let results = lexer.collect::<Vec<TokenData>>();

	assert_eq!(expected, results);
}

#[test]
fn if_else_stmt_token_read() {
	let input = r#"if (5 < 10) {
        return true;
    } else {
        return false;
    }"#;
	let expected = vec![
		TokenData::new(Token::If, 0, input.as_bytes()),
		TokenData::new(Token::OpenParens, 3, input.as_bytes()),
		TokenData::new(Token::Integer("5".into()), 4, input.as_bytes()),
		TokenData::new(Token::LessThan, 6, input.as_bytes()),
		TokenData::new(Token::Integer("10".into()), 8, input.as_bytes()),
		TokenData::new(Token::CloseParens, 10, input.as_bytes()),
		TokenData::new(Token::OpenCurlyBraces, 12, input.as_bytes()),
		TokenData::new(Token::Return, 22, input.as_bytes()),
		TokenData::new(Token::True, 29, input.as_bytes()),
		TokenData::new(Token::Semicolon, 33, input.as_bytes()),
		TokenData::new(Token::CloseCurlyBraces, 39, input.as_bytes()),
		TokenData::new(Token::Else, 41, input.as_bytes()),
		TokenData::new(Token::OpenCurlyBraces, 46, input.as_bytes()),
		TokenData::new(Token::Return, 56, input.as_bytes()),
		TokenData::new(Token::False, 63, input.as_bytes()),
		TokenData::new(Token::Semicolon, 68, input.as_bytes()),
		TokenData::new(Token::CloseCurlyBraces, 74, input.as_bytes()),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn eq_neq_token_read() {
	let input = r#"
    10 == 10;
    10 != 9;
    "#;
	let expected = vec![
		TokenData::new(Token::Integer("10".into()), 5, input.as_bytes()),
		TokenData::new(Token::DoubleEqual, 8, input.as_bytes()),
		TokenData::new(Token::Integer("10".into()), 11, input.as_bytes()),
		TokenData::new(Token::Semicolon, 13, input.as_bytes()),
		TokenData::new(Token::Integer("10".into()), 19, input.as_bytes()),
		TokenData::new(Token::NotEqual, 22, input.as_bytes()),
		TokenData::new(Token::Integer("9".into()), 25, input.as_bytes()),
		TokenData::new(Token::Semicolon, 26, input.as_bytes()),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn single_quote_string() {
	let input = "'some string here'";
	let expected = vec![TokenData::new(
		Token::String("'some string here'".into()),
		0,
		input.as_bytes(),
	)];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn double_quote_string() {
	let input = r#""some string here""#;
	let expected = vec![TokenData::new(
		Token::String(r#""some string here""#.into()),
		0,
		input.as_bytes(),
	)];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}
