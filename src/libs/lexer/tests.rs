use super::{
	super::token::{Token, TokenData},
	Lexer,
};

#[test]
fn simple_token_read() {
	let input = "=+(){},; \t";
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	let input = input.as_bytes();
	let expected = vec![
		TokenData::new(Token::Equal, 0, input),
		TokenData::new(Token::Plus, 1, input),
		TokenData::new(Token::OpenParens, 2, input),
		TokenData::new(Token::CloseParens, 3, input),
		TokenData::new(Token::OpenCurlyBraces, 4, input),
		TokenData::new(Token::CloseCurlyBraces, 5, input),
		TokenData::new(Token::Comma, 6, input),
		TokenData::new(Token::Semicolon, 7, input),
	];
	assert_eq!(results, expected);
}

#[test]
fn let_stmt_token_read() {
	let input = "let param = 10;";
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	let input = input.as_bytes();
	let expected = vec![
		TokenData::new(Token::Let, 0, input),
		TokenData::new(Token::Identifier("param".into()), 4, input),
		TokenData::new(Token::Equal, 10, input),
		TokenData::new(Token::Integer("10".into()), 12, input),
		TokenData::new(Token::Semicolon, 14, input),
	];
	assert_eq!(results, expected);
}

#[test]
fn bang_sign_token_read() {
	let input = r#"!"#;
	let expected = vec![TokenData::new(Token::Bang, 0, input.as_bytes())];

	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(results, expected);
}

#[test]
fn math_signs_token_read() {
	let input = r#"
    !-/*5;
    5 < 10 > 5;
    "#;
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	let input = input.as_bytes();
	let expected = vec![
		TokenData::new(Token::Bang, 5, input),
		TokenData::new(Token::Minus, 6, input),
		TokenData::new(Token::Slash, 7, input),
		TokenData::new(Token::Asterisk, 8, input),
		TokenData::new(Token::Integer("5".into()), 9, input),
		TokenData::new(Token::Semicolon, 10, input),
		TokenData::new(Token::Integer("5".into()), 16, input),
		TokenData::new(Token::LessThan, 18, input),
		TokenData::new(Token::Integer("10".into()), 20, input),
		TokenData::new(Token::GreaterThan, 23, input),
		TokenData::new(Token::Integer("5".into()), 25, input),
		TokenData::new(Token::Semicolon, 26, input),
	];
	assert_eq!(results, expected);
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
	let lexer = Lexer::new(input);
	let results = lexer.collect::<Vec<TokenData>>();

	let input = input.as_bytes();
	let expected = vec![
		TokenData::new(Token::Let, 0, input),
		TokenData::new(Token::Identifier("five".into()), 4, input),
		TokenData::new(Token::Equal, 9, input),
		TokenData::new(Token::Integer("5".into()), 11, input),
		TokenData::new(Token::Semicolon, 12, input),
		TokenData::new(Token::Let, 18, input),
		TokenData::new(Token::Identifier("ten".into()), 22, input),
		TokenData::new(Token::Equal, 26, input),
		TokenData::new(Token::Integer("10".into()), 28, input),
		TokenData::new(Token::Semicolon, 30, input),
		TokenData::new(Token::Let, 37, input),
		TokenData::new(Token::Identifier("add".into()), 41, input),
		TokenData::new(Token::Equal, 45, input),
		TokenData::new(Token::Function, 47, input),
		TokenData::new(Token::OpenParens, 55, input),
		TokenData::new(Token::Identifier("x".into()), 56, input),
		TokenData::new(Token::Comma, 57, input),
		TokenData::new(Token::Identifier("y".into()), 59, input),
		TokenData::new(Token::CloseParens, 60, input),
		TokenData::new(Token::OpenCurlyBraces, 62, input),
		TokenData::new(Token::Identifier("x".into()), 70, input),
		TokenData::new(Token::Plus, 72, input),
		TokenData::new(Token::Identifier("y".into()), 74, input),
		TokenData::new(Token::Semicolon, 75, input),
		TokenData::new(Token::CloseCurlyBraces, 81, input),
		TokenData::new(Token::Semicolon, 82, input),
		TokenData::new(Token::Let, 89, input),
		TokenData::new(Token::Identifier("result".into()), 93, input),
		TokenData::new(Token::Equal, 100, input),
		TokenData::new(Token::Identifier("add".into()), 102, input),
		TokenData::new(Token::OpenParens, 105, input),
		TokenData::new(Token::Identifier("five".into()), 106, input),
		TokenData::new(Token::Comma, 110, input),
		TokenData::new(Token::Identifier("ten".into()), 112, input),
		TokenData::new(Token::CloseParens, 115, input),
		TokenData::new(Token::Semicolon, 116, input),
	];

	assert_eq!(results, expected);
}

#[test]
fn if_else_stmt_token_read() {
	let input = r#"if (5 < 10) {
        return true;
    } else {
        return false;
    }"#;
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	let input = input.as_bytes();
	let expected = vec![
		TokenData::new(Token::If, 0, input),
		TokenData::new(Token::OpenParens, 3, input),
		TokenData::new(Token::Integer("5".into()), 4, input),
		TokenData::new(Token::LessThan, 6, input),
		TokenData::new(Token::Integer("10".into()), 8, input),
		TokenData::new(Token::CloseParens, 10, input),
		TokenData::new(Token::OpenCurlyBraces, 12, input),
		TokenData::new(Token::Return, 22, input),
		TokenData::new(Token::True, 29, input),
		TokenData::new(Token::Semicolon, 33, input),
		TokenData::new(Token::CloseCurlyBraces, 39, input),
		TokenData::new(Token::Else, 41, input),
		TokenData::new(Token::OpenCurlyBraces, 46, input),
		TokenData::new(Token::Return, 56, input),
		TokenData::new(Token::False, 63, input),
		TokenData::new(Token::Semicolon, 68, input),
		TokenData::new(Token::CloseCurlyBraces, 74, input),
	];
	assert_eq!(results, expected);
}

#[test]
fn eq_neq_token_read() {
	let input = r#"
    10 == 10;
    10 != 9;
    10 >= 9;
    9 <= 10;
    "#;
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	let input = input.as_bytes();
	let expected = vec![
		TokenData::new(Token::Integer("10".into()), 5, input),
		TokenData::new(Token::DoubleEqual, 8, input),
		TokenData::new(Token::Integer("10".into()), 11, input),
		TokenData::new(Token::Semicolon, 13, input),
		TokenData::new(Token::Integer("10".into()), 19, input),
		TokenData::new(Token::NotEqual, 22, input),
		TokenData::new(Token::Integer("9".into()), 25, input),
		TokenData::new(Token::Semicolon, 26, input),
		TokenData::new(Token::Integer("10".into()), 32, input),
		TokenData::new(Token::GreaterThanOrEqual, 35, input),
		TokenData::new(Token::Integer("9".into()), 38, input),
		TokenData::new(Token::Semicolon, 39, input),
		TokenData::new(Token::Integer("9".into()), 45, input),
		TokenData::new(Token::LessThanOrEqual, 47, input),
		TokenData::new(Token::Integer("10".into()), 50, input),
		TokenData::new(Token::Semicolon, 52, input),
	];
	assert_eq!(results, expected);
}

#[test]
fn single_quote_string() {
	let input = r#"'some string here'"#;
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

	assert_eq!(results, expected);
}
