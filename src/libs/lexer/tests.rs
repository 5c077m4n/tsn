use super::{
	super::token::{Token, TokenData},
	Lexer,
};

#[test]
fn simple_token_read() {
	let input = "=+(){},; \t";
	let expected = vec![
		TokenData::new(Token::Equal, 0),
		TokenData::new(Token::Plus, 1),
		TokenData::new(Token::OpenParens, 2),
		TokenData::new(Token::CloseParens, 3),
		TokenData::new(Token::OpenCurlyBraces, 4),
		TokenData::new(Token::CloseCurlyBraces, 5),
		TokenData::new(Token::Comma, 6),
		TokenData::new(Token::Semicolon, 7),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}

#[test]
fn let_stmt_token_read() {
	let input = "let param = 10;";
	let expected = vec![
		TokenData::new(Token::Let, 0),
		TokenData::new(Token::Identifier("param".into()), 4),
		TokenData::new(Token::Equal, 10),
		TokenData::new(Token::Integer("10".into()), 12),
		TokenData::new(Token::Semicolon, 14),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results, "input `{}`", input);
}

#[test]
fn bang_sign_token_read() {
	let input = r#"!"#;
	let expected = vec![TokenData::new(Token::Bang, 0)];

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
		TokenData::new(Token::Bang, 5),
		TokenData::new(Token::Minus, 6),
		TokenData::new(Token::Slash, 7),
		TokenData::new(Token::Asterisk, 8),
		TokenData::new(Token::Integer("5".into()), 9),
		TokenData::new(Token::Semicolon, 10),
		TokenData::new(Token::Integer("5".into()), 16),
		TokenData::new(Token::LessThan, 18),
		TokenData::new(Token::Integer("10".into()), 20),
		TokenData::new(Token::GreaterThan, 23),
		TokenData::new(Token::Integer("5".into()), 25),
		TokenData::new(Token::Semicolon, 26),
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
		TokenData::new(Token::Let, 0),
		TokenData::new(Token::Identifier("five".into()), 4),
		TokenData::new(Token::Equal, 9),
		TokenData::new(Token::Integer("5".into()), 11),
		TokenData::new(Token::Semicolon, 12),
		TokenData::new(Token::Let, 18),
		TokenData::new(Token::Identifier("ten".into()), 22),
		TokenData::new(Token::Equal, 26),
		TokenData::new(Token::Integer("10".into()), 28),
		TokenData::new(Token::Semicolon, 30),
		TokenData::new(Token::Let, 37),
		TokenData::new(Token::Identifier("add".into()), 41),
		TokenData::new(Token::Equal, 45),
		TokenData::new(Token::Function, 47),
		TokenData::new(Token::OpenParens, 55),
		TokenData::new(Token::Identifier("x".into()), 56),
		TokenData::new(Token::Comma, 57),
		TokenData::new(Token::Identifier("y".into()), 59),
		TokenData::new(Token::CloseParens, 60),
		TokenData::new(Token::OpenCurlyBraces, 62),
		TokenData::new(Token::Identifier("x".into()), 70),
		TokenData::new(Token::Plus, 72),
		TokenData::new(Token::Identifier("y".into()), 74),
		TokenData::new(Token::Semicolon, 75),
		TokenData::new(Token::CloseCurlyBraces, 81),
		TokenData::new(Token::Semicolon, 82),
		TokenData::new(Token::Let, 89),
		TokenData::new(Token::Identifier("result".into()), 93),
		TokenData::new(Token::Equal, 100),
		TokenData::new(Token::Identifier("add".into()), 102),
		TokenData::new(Token::OpenParens, 105),
		TokenData::new(Token::Identifier("five".into()), 106),
		TokenData::new(Token::Comma, 110),
		TokenData::new(Token::Identifier("ten".into()), 112),
		TokenData::new(Token::CloseParens, 115),
		TokenData::new(Token::Semicolon, 116),
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
		TokenData::new(Token::If, 0),
		TokenData::new(Token::OpenParens, 3),
		TokenData::new(Token::Integer("5".into()), 4),
		TokenData::new(Token::LessThan, 6),
		TokenData::new(Token::Integer("10".into()), 8),
		TokenData::new(Token::CloseParens, 10),
		TokenData::new(Token::OpenCurlyBraces, 12),
		TokenData::new(Token::Return, 22),
		TokenData::new(Token::True, 29),
		TokenData::new(Token::Semicolon, 33),
		TokenData::new(Token::CloseCurlyBraces, 39),
		TokenData::new(Token::Else, 41),
		TokenData::new(Token::OpenCurlyBraces, 46),
		TokenData::new(Token::Return, 56),
		TokenData::new(Token::False, 63),
		TokenData::new(Token::Semicolon, 68),
		TokenData::new(Token::CloseCurlyBraces, 74),
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
		TokenData::new(Token::Integer("10".into()), 5),
		TokenData::new(Token::DoubleEqual, 8),
		TokenData::new(Token::Integer("10".into()), 11),
		TokenData::new(Token::Semicolon, 13),
		TokenData::new(Token::Integer("10".into()), 19),
		TokenData::new(Token::NEq, 22),
		TokenData::new(Token::Integer("9".into()), 25),
		TokenData::new(Token::Semicolon, 26),
	];
	let lexer = Lexer::new(input);
	let results: Vec<TokenData> = lexer.collect();

	assert_eq!(expected, results);
}
