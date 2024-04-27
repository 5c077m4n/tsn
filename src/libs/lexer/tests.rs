use super::{super::token::Token, Lexer};

#[test]
fn simple_token_read() {
	let input = "=+(){},;";
	let expected = vec![
		Token::Eq,
		Token::Plus,
		Token::OpenParens,
		Token::CloseParens,
		Token::OpenCurlyBraces,
		Token::CloseCurlyBraces,
		Token::Comma,
		Token::Semicolon,
	];
	let lexer = Lexer::new(input);
	let results: Vec<Token> = lexer.into_iter().collect();

	assert_eq!(expected, results);
}

#[test]
fn let_stmt_token_read() {
	let input = "let param = 10;";
	let expected = vec![
		Token::Let,
		Token::Identifier(b"param".into()),
		Token::Eq,
		Token::Integer(b"10".into()),
		Token::Semicolon,
	];
	let lexer = Lexer::new(input);
	let results: Vec<Token> = lexer.into_iter().collect();

	assert_eq!(expected, results);
}

#[test]
fn math_signs_token_read() {
	let input = r#"
    !-/*5;
    5 < 10 > 5;
    "#;
	let expected = vec![
		Token::Bang,
		Token::Minus,
		Token::Slash,
		Token::Asterisk,
		Token::Integer(b"5".into()),
		Token::Semicolon,
		Token::Integer(b"5".into()),
		Token::LT,
		Token::Integer(b"10".into()),
		Token::GT,
		Token::Integer(b"5".into()),
		Token::Semicolon,
	];
	let lexer = Lexer::new(input);
	let results: Vec<Token> = lexer.into_iter().collect();

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
		Token::Let,
		Token::Identifier(b"five".into()),
		Token::Eq,
		Token::Integer(b"5".into()),
		Token::Semicolon,
		Token::Let,
		Token::Identifier(b"ten".into()),
		Token::Eq,
		Token::Integer(b"10".into()),
		Token::Semicolon,
		Token::Let,
		Token::Identifier(b"add".into()),
		Token::Eq,
		Token::Function,
		Token::OpenParens,
		Token::Identifier(b"x".into()),
		Token::Comma,
		Token::Identifier(b"y".into()),
		Token::CloseParens,
		Token::OpenCurlyBraces,
		Token::Identifier(b"x".into()),
		Token::Plus,
		Token::Identifier(b"y".into()),
		Token::Semicolon,
		Token::CloseCurlyBraces,
		Token::Semicolon,
		Token::Let,
		Token::Identifier(b"result".into()),
		Token::Eq,
		Token::Identifier(b"add".into()),
		Token::OpenParens,
		Token::Identifier(b"five".into()),
		Token::Comma,
		Token::Identifier(b"ten".into()),
		Token::CloseParens,
		Token::Semicolon,
	];
	let lexer = Lexer::new(input);
	let results: Vec<Token> = lexer.into_iter().collect();

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
		Token::If,
		Token::OpenParens,
		Token::Integer(b"5".into()),
		Token::LT,
		Token::Integer(b"10".into()),
		Token::CloseParens,
		Token::OpenCurlyBraces,
		Token::Return,
		Token::True,
		Token::Semicolon,
		Token::CloseCurlyBraces,
		Token::Else,
		Token::OpenCurlyBraces,
		Token::Return,
		Token::False,
		Token::Semicolon,
		Token::CloseCurlyBraces,
	];
	let lexer = Lexer::new(input);
	let results: Vec<Token> = lexer.into_iter().collect();

	assert_eq!(expected, results);
}

#[test]
fn eq_neq_token_read() {
	let input = r#"
    10 == 10;
    10 != 9;
    "#;
	let expected = vec![
		Token::Integer(b"10".into()),
		Token::EqEq,
		Token::Integer(b"10".into()),
		Token::Semicolon,
		Token::Integer(b"10".into()),
		Token::NEq,
		Token::Integer(b"9".into()),
		Token::Semicolon,
	];
	let lexer = Lexer::new(input);
	let results: Vec<Token> = lexer.into_iter().collect();

	assert_eq!(expected, results);
}
