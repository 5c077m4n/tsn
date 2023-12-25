use anyhow::{bail, Error, Result};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(u8),
    /// End of file
    EOF,
    Identifier(&'static [u8]),
    Integer(&'static [u8]),
    /// `=`
    Eq,
    /// `+`
    Plus,
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
    Function,
    Let,
    Const,
}

impl From<&'static [u8]> for Token {
    fn from(value: &'static [u8]) -> Self {
        match value {
            b"function" => Token::Function,
            b"let" => Token::Let,
            b"const" => Token::Const,
            other => Token::Identifier(other),
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
            b'(' => Ok(Token::OpenParens),
            b')' => Ok(Token::CloseParens),
            b'{' => Ok(Token::OpenCurlyBraces),
            b'}' => Ok(Token::CloseCurlyBraces),
            b';' => Ok(Token::Semicolon),
            b'\0' => Ok(Token::EOF),
            _ => bail!("Not found"),
        }
    }
}
