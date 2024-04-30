use super::token::{Token, TokenData};

fn is_letter(c: u8) -> bool {
	c.is_ascii_alphabetic() || c == b'_' || c == b'$'
}

pub struct Lexer {
	input: Vec<u8>,
	position: usize,
	read_position: usize,
	c: Option<u8>,
}

impl Lexer {
	pub fn new(input: &str) -> Self {
		let mut s = Self {
			input: input.as_bytes().to_vec(),
			position: 0,
			read_position: 0,
			c: None,
		};
		s.read_char();
		s
	}

	fn read_char(&mut self) {
		self.c = self.input.get(self.read_position).copied();
		self.position = self.read_position;
		self.read_position += 1;
	}

	fn read_identifier(&mut self) -> &[u8] {
		let position = self.position;
		while self.c.is_some_and(|c| is_letter(c) || c.is_ascii_digit()) {
			self.read_char();
		}
		&self.input[position..self.position]
	}

	fn read_number(&mut self) -> &[u8] {
		let position = self.position;
		while self.c.is_some_and(|c| c.is_ascii_digit()) {
			self.read_char();
		}
		&self.input[position..self.position]
	}

	fn peek_char(&mut self) -> u8 {
		*self.input.get(self.read_position).unwrap_or(&b'\0')
	}

	pub fn next_token(&mut self) -> TokenData {
		let c = self.c.unwrap_or(b'\0');
		let peek = self.peek_char();
		let start_position = self.position;

		let token = match (c, peek) {
			(b'=', b'=') => {
				self.read_char();
				Some(Token::DoubleEqual)
			}
			(b'=', _) => Some(Token::Equal),
			(b',', _) => Some(Token::Comma),
			(b'+', _) => Some(Token::Plus),
			(b'-', _) => Some(Token::Minus),
			(b'!', b'=') => {
				self.read_char();
				Some(Token::NEq)
			}
			(b'!', _) => Some(Token::Bang),
			(b'*', _) => Some(Token::Asterisk),
			(b'/', _) => Some(Token::Slash),
			(b'&', _) => Some(Token::Ampersand),
			(b'|', _) => Some(Token::Pipe),
			(b'<', b'=') => {
				self.read_char();
				Some(Token::LessThanOrEqual)
			}
			(b'<', _) => Some(Token::LessThan),
			(b'>', b'=') => {
				self.read_char();
				Some(Token::GreaterThanOrEqual)
			}
			(b'>', _) => Some(Token::GreaterThan),
			(b'(', _) => Some(Token::OpenParens),
			(b')', _) => Some(Token::CloseParens),
			(b'[', _) => Some(Token::OpenSquareBraces),
			(b']', _) => Some(Token::CloseSquareBraces),
			(b'{', _) => Some(Token::OpenCurlyBraces),
			(b'}', _) => Some(Token::CloseCurlyBraces),
			(b':', _) => Some(Token::Colon),
			(b';', _) => Some(Token::Semicolon),
			(b' ', _) => Some(Token::Space),
			(b'\t', _) => Some(Token::Tab),
			(b'\r', _) => Some(Token::CarrigeReturn),
			(b'\n', _) => Some(Token::NewLine),
			(b'\0', _) => Some(Token::EndOfFile),
			_ => None,
		};

		let token = if let Some(token) = token {
			self.read_char();
			token
		} else if is_letter(c) {
			let ident = self.read_identifier();
			Token::from(ident)
		} else if c.is_ascii_digit() {
			let number = self.read_number();
			match String::from_utf8(number.to_owned()) {
				Ok(n) => Token::Integer(n),
				Err(_) => Token::Illegal(c.to_string()),
			}
		} else {
			Token::Illegal(c.to_string())
		};

		TokenData::new(token, start_position)
	}
}

impl Iterator for Lexer {
	type Item = TokenData;

	fn next(&mut self) -> Option<Self::Item> {
		let mut next_token = self.next_token();
		while next_token.is_whitespace() {
			next_token = self.next_token();
		}

		if !next_token.is_end_of_file() {
			Some(next_token)
		} else {
			None
		}
	}
}

#[cfg(test)]
mod tests;
