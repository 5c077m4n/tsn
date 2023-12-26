use super::{super::token::Token, Lexer};

#[test]
fn simple_token_read() {
    let input = "=+(){},;";
    let expected = &[
        Token::Eq,
        Token::Plus,
        Token::OpenParens,
        Token::CloseParens,
        Token::OpenCurlyBraces,
        Token::CloseCurlyBraces,
        Token::Comma,
        Token::Semicolon,
    ];
    let mut lexer = Lexer::new(input);

    for (index, expected_token) in expected.iter().enumerate() {
        let next_token = lexer.next_token();
        assert_eq!(next_token, *expected_token, "expected token #{}", index);
    }
}

#[test]
fn let_stmt_token_read() {
    let input = "let param = 10;";
    let expected = &[
        Token::Let,
        Token::Identifier(b"param"),
        Token::Eq,
        Token::Integer(b"10"),
        Token::Semicolon,
        Token::EOF,
    ];
    let mut lexer = Lexer::new(input);

    for (index, expected_token) in expected.iter().enumerate() {
        let next_token = lexer.next_token();
        assert_eq!(next_token, *expected_token, "expected token #{}", index + 1);
    }
}

#[test]
fn math_signs_token_read() {
    let input = r#"
    !-/*5;
    5 < 10 > 5;
    "#;
    let expected = &[
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Integer(b"5"),
        Token::Semicolon,
        Token::Integer(b"5"),
        Token::LT,
        Token::Integer(b"10"),
        Token::GT,
        Token::Integer(b"5"),
        Token::Semicolon,
        Token::EOF,
    ];
    let mut lexer = Lexer::new(input);

    for (index, expected_token) in expected.iter().enumerate() {
        let next_token = lexer.next_token();
        assert_eq!(next_token, *expected_token, "expected token #{}", index);
    }
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
    let expected = &[
        Token::Let,
        Token::Identifier(b"five"),
        Token::Eq,
        Token::Integer(b"5"),
        Token::Semicolon,
        Token::Let,
        Token::Identifier(b"ten"),
        Token::Eq,
        Token::Integer(b"10"),
        Token::Semicolon,
        Token::Let,
        Token::Identifier(b"add"),
        Token::Eq,
        Token::Function,
        Token::OpenParens,
        Token::Identifier(b"x"),
        Token::Comma,
        Token::Identifier(b"y"),
        Token::CloseParens,
        Token::OpenCurlyBraces,
        Token::Identifier(b"x"),
        Token::Plus,
        Token::Identifier(b"y"),
        Token::Semicolon,
        Token::CloseCurlyBraces,
        Token::Semicolon,
        Token::Let,
        Token::Identifier(b"result"),
        Token::Eq,
        Token::Identifier(b"add"),
        Token::OpenParens,
        Token::Identifier(b"five"),
        Token::Comma,
        Token::Identifier(b"ten"),
        Token::CloseParens,
        Token::Semicolon,
        Token::EOF,
    ];
    let mut lexer = Lexer::new(input);

    for (index, expected_token) in expected.iter().enumerate() {
        let next_token = lexer.next_token();
        assert_eq!(next_token, *expected_token, "expected token #{}", index + 1);
    }
}

#[test]
fn if_else_stmt_token_read() {
    let input = r#"if (5 < 10) {
        return true;
    } else {
        return false;
    }"#;
    let expected = &[
        Token::If,
        Token::OpenParens,
        Token::Integer(b"5"),
        Token::LT,
        Token::Integer(b"10"),
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
    let mut lexer = Lexer::new(input);

    for (index, expected_token) in expected.iter().enumerate() {
        let next_token = lexer.next_token();
        assert_eq!(next_token, *expected_token, "expected token #{}", index + 1);
    }
}

#[test]
fn eq_neq_token_read() {
    let input = r#"
    10 == 10;
    10 != 9;
    "#;
    let expected = &[
        Token::Integer(b"10"),
        Token::EqEq,
        Token::Integer(b"10"),
        Token::Semicolon,
        Token::Integer(b"10"),
        Token::NEq,
        Token::Integer(b"9"),
        Token::Semicolon,
    ];
    let mut lexer = Lexer::new(input);

    for (index, expected_token) in expected.iter().enumerate() {
        let next_token = lexer.next_token();
        assert_eq!(next_token, *expected_token, "expected token #{}", index + 1);
    }
}
