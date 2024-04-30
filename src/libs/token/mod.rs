use std::fmt::{self, Display};

use strum_macros::{EnumDiscriminants, EnumIter};

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter))]
#[strum_discriminants(name(TokenType))]
pub enum Token {
	Illegal(String),
	EndOfFile,
	Identifier(String),
	Integer(String),
	String(String),
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
	/// `'`
	Quote,
	/// `"`
	DoubleQuote,
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
			Self::String(s) => write!(f, "{}", s),
			Self::Equal => write!(f, "="),
			Self::Plus => write!(f, "+"),
			Self::Minus => write!(f, "-"),
			Self::Bang => write!(f, "!"),
			Self::Asterisk => write!(f, "*"),
			Self::Slash => write!(f, "/"),
			Self::Ampersand => write!(f, "&"),
			Self::Pipe => write!(f, "|"),
			Self::Quote => write!(f, "'"),
			Self::DoubleQuote => write!(f, "\""),
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Location {
	start_line: usize,
	start_column: usize,
	end_line: usize,
	end_column: usize,
}
impl Display for Location {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let Self {
			start_line,
			start_column,
			end_line,
			end_column,
		} = self;
		write!(
			f,
			"{}:{}-{}:{}",
			start_line, start_column, end_line, end_column
		)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenData {
	token: Token,
	location: Location,
}
impl TokenData {
	pub fn new(token: Token, start_position: usize, input: &[u8]) -> Self {
		let end_position = start_position + token.to_string().len();

		let mut loc = Location::default();
		let mut counter = 0;

		for (index, line) in input.split(|&c| c == b'\n').enumerate() {
			if (counter..counter + line.len()).contains(&start_position) {
				loc.start_line = index + 1;
				loc.start_column = start_position - counter + 1;
			}
			if (counter..=counter + line.len()).contains(&end_position) {
				loc.end_line = index + 1;
				loc.end_column = end_position - counter + 1;
			}
			counter += line.len();
		}

		Self {
			token,
			location: loc,
		}
	}

	pub fn token(&self) -> &Token {
		&self.token
	}
	pub fn location(&self) -> &Location {
		&self.location
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
