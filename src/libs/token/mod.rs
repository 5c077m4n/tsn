use std::fmt;

use strum_macros::{EnumDiscriminants, EnumIter};

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter))]
#[strum_discriminants(name(TokenType))]
pub enum Token {
	Illegal(String),
	EndOfFile,
	Identifier(String),
	Integer(String),
	/// `=`
	Equal,
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
	/// `&`
	Ampersand,
	/// `|`
	Pipe,
	/// `<`
	LessThan,
	/// `<=`
	LessThanOrEqual,
	/// `>`
	GreaterThan,
	/// `>=`
	GreaterThanOrEqual,
	/// `,`
	Comma,
	/// `:`
	Colon,
	/// `;`
	Semicolon,
	/// `(`
	OpenParens,
	/// `)`
	CloseParens,
	/// `[`
	OpenSquareBraces,
	/// `]`
	CloseSquareBraces,
	/// `{`
	OpenCurlyBraces,
	/// `}`
	CloseCurlyBraces,
	/// `==`
	DoubleEqual,
	/// `!=`
	NEq,

	/// ` `
	Space,
	/// `\t`
	Tab,
	/// `\r`
	CarrigeReturn,
	/// `\n`
	NewLine,

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

impl From<&[u8]> for Token {
	fn from(value: &[u8]) -> Self {
		match value {
			b"function" => Token::Function,
			b"let" => Token::Let,
			b"const" => Token::Const,
			b"if" => Token::If,
			b"else" => Token::Else,
			b"return" => Token::Return,
			b"true" => Token::True,
			b"false" => Token::False,
			other => Token::Identifier(
				String::from_utf8(other.to_owned())
					.expect("Could not cast the `&[u8]` to a string"),
			),
		}
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Illegal(c) => write!(f, "{}", c),
			Self::Space => write!(f, " "),
			Self::Tab => write!(f, "\t"),
			Self::CarrigeReturn => write!(f, "\r"),
			Self::NewLine => writeln!(f),
			Self::EndOfFile => write!(f, "EOF"),
			Self::Identifier(ident) => write!(f, "{}", ident),
			Self::Integer(n) => write!(f, "{}", n),
			Self::Equal => write!(f, "="),
			Self::Plus => write!(f, "+"),
			Self::Minus => write!(f, "-"),
			Self::Bang => write!(f, "!"),
			Self::Asterisk => write!(f, "*"),
			Self::Slash => write!(f, "/"),
			Self::Ampersand => write!(f, "&"),
			Self::Pipe => write!(f, "|"),
			Self::LessThan => write!(f, "<"),
			Self::LessThanOrEqual => write!(f, "<="),
			Self::GreaterThan => write!(f, ">"),
			Self::GreaterThanOrEqual => write!(f, ">="),
			Self::Comma => write!(f, ","),
			Self::Colon => write!(f, ":"),
			Self::Semicolon => write!(f, ";"),
			Self::OpenParens => write!(f, "("),
			Self::CloseParens => write!(f, ")"),
			Self::OpenCurlyBraces => write!(f, "{{"),
			Self::CloseCurlyBraces => write!(f, "}}"),
			Self::OpenSquareBraces => write!(f, "["),
			Self::CloseSquareBraces => write!(f, "]"),
			Self::DoubleEqual => write!(f, "=="),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenData {
	token: Token,
	position: usize,
}
impl TokenData {
	pub fn new(token: Token, position: usize) -> Self {
		Self { token, position }
	}

	pub fn token(&self) -> &Token {
		&self.token
	}
	pub fn position(&self) -> usize {
		self.position
	}
	pub fn is_whitespace(&self) -> bool {
		matches!(
			self.token,
			Token::Space | Token::Tab | Token::CarrigeReturn | Token::NewLine
		)
	}
	pub fn is_end_of_file(&self) -> bool {
		self.token == Token::EndOfFile
	}
}
