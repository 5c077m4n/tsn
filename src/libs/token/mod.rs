use std::fmt;

use anyhow::{bail, Error, Result};
use strum_macros::{EnumDiscriminants, EnumIter};

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter))]
#[strum_discriminants(name(TokenType))]
pub enum Token {
	Illegal(u8),
	/// End of file
	EndOfFile,
	Identifier(Vec<u8>),
	Integer(Vec<u8>),
	/// `=`
	Eq,
	/// `+`
	Plus,
	/// `-`
	Minus,
	/// `!`
	Bang,
	/// `*`
	Asterisk,
	/// `/`
	Slash,
	/// Less than `<`
	LT,
	/// Greater than `>`
	GT,
	/// `,`
	Comma,
	/// `;`
	Semicolon,
	/// `(`
	OpenParens,
	/// `)`
	CloseParens,
	/// `{`
	OpenCurlyBraces,
	/// `}`
	CloseCurlyBraces,
	/// `==`
	EqEq,
	/// `!=`
	NEq,

	// Keywords
	Function,
	Let,
	Const,
	If,
	Else,
	Return,
	True,
	False,
}

impl From<Vec<u8>> for Token {
	fn from(value: Vec<u8>) -> Self {
		match value.as_slice() {
			b"function" => Token::Function,
			b"let" => Token::Let,
			b"const" => Token::Const,
			b"if" => Token::If,
			b"else" => Token::Else,
			b"return" => Token::Return,
			b"true" => Token::True,
			b"false" => Token::False,
			b"==" => Token::EqEq,
			b"!=" => Token::NEq,
			other => Token::Identifier(other.to_vec()),
		}
	}
}

impl TryFrom<u8> for Token {
	type Error = Error;
	fn try_from(value: u8) -> Result<Self, Self::Error> {
		match value {
			b'=' => Ok(Token::Eq),
			b',' => Ok(Token::Comma),
			b'+' => Ok(Token::Plus),
			b'-' => Ok(Token::Minus),
			b'!' => Ok(Token::Bang),
			b'*' => Ok(Token::Asterisk),
			b'/' => Ok(Token::Slash),
			b'<' => Ok(Token::LT),
			b'>' => Ok(Token::GT),
			b'(' => Ok(Token::OpenParens),
			b')' => Ok(Token::CloseParens),
			b'{' => Ok(Token::OpenCurlyBraces),
			b'}' => Ok(Token::CloseCurlyBraces),
			b';' => Ok(Token::Semicolon),
			b'\0' => Ok(Token::EndOfFile),
			_ => bail!("Not found"),
		}
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Illegal(c) => write!(f, "{}", *c as char),
			Self::EndOfFile => write!(f, "EOF"),
			Self::Identifier(ident) => {
				let ident = String::from_utf8(ident.to_owned()).map_err(|_| fmt::Error)?;
				write!(f, "{}", ident)
			}
			Self::Integer(n) => {
				let n = String::from_utf8(n.to_owned()).map_err(|_| fmt::Error)?;
				write!(f, "{}", n)
			}
			Self::Eq => write!(f, "="),
			Self::Plus => write!(f, "+"),
			Self::Minus => write!(f, "-"),
			Self::Bang => write!(f, "!"),
			Self::Asterisk => write!(f, "*"),
			Self::Slash => write!(f, "/"),
			Self::LT => write!(f, "<"),
			Self::GT => write!(f, ">"),
			Self::Comma => write!(f, ","),
			Self::Semicolon => write!(f, ";"),
			Self::OpenParens => write!(f, "("),
			Self::CloseParens => write!(f, ")"),
			Self::OpenCurlyBraces => write!(f, "{{"),
			Self::CloseCurlyBraces => write!(f, "}}"),
			Self::EqEq => write!(f, "=="),
			Self::NEq => write!(f, "!="),
			Self::Function => write!(f, "function"),
			Self::Let => write!(f, "let"),
			Self::Const => write!(f, "const"),
			Self::If => write!(f, "if"),
			Self::Else => write!(f, "else"),
			Self::Return => write!(f, "return"),
			Self::True => write!(f, "true"),
			Self::False => write!(f, "false"),
		}
	}
}
