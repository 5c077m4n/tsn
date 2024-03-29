use super::token::Token;

fn is_letter(c: u8) -> bool {
	b'a' <= c && c <= b'z' || b'A' <= c && c <= b'Z' || c == b'_' || c == b'$'
}
fn is_digit(c: u8) -> bool {
	b'0' <= c && c <= b'9'
}
fn is_whitespace(c: u8) -> bool {
	c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

pub struct Lexer {
	input: Vec<u8>,
	position: usize,
	read_position: usize,
	c: Option<u8>,
}

impl Lexer {
	pub fn new(input: Box<str>) -> Self {
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
		self.c = self.input.iter().nth(self.read_position).copied();
		self.position = self.read_position;
		self.read_position += 1;
	}

	fn read_identifier(&mut self) -> Vec<u8> {
		let position = self.position;
		while self.c.map_or(false, is_letter) {
			self.read_char();
		}
		self.input[position..self.position].to_vec()
	}

	fn read_number(&mut self) -> Vec<u8> {
		let position = self.position;
		while self.c.map_or(false, is_digit) {
			self.read_char();
		}
		self.input[position..self.position].to_vec()
	}

	fn skip_whitespce(&mut self) {
		while self.c.map_or(false, is_whitespace) {
			self.read_char();
		}
	}

	fn peek_char(&mut self) -> u8 {
		*self.input.get(self.read_position).unwrap_or(&b'\0')
	}

	pub fn next_token(&mut self) -> Token {
		self.skip_whitespce();

		let c = self.c.unwrap_or(b'\0');
		let peek = self.peek_char();

		let token = if peek == b'=' || peek == b'!' {
			self.read_char();

			let literal = self.input[(self.position - 1)..self.read_position].to_vec();
			Ok(Token::from(literal))
		} else {
			Token::try_from(c)
		};

		match token {
			Ok(t) => {
				self.read_char();
				t
			}
			Err(_) => {
				if is_letter(c) {
					let ident = self.read_identifier();
					Token::from(ident)
				} else if is_digit(c) {
					let number = self.read_number();
					Token::Integer(number)
				} else {
					Token::Illegal(c)
				}
			}
		}
	}
}

impl Iterator for Lexer {
	type Item = Token;
	fn next(&mut self) -> Option<Self::Item> {
		match self.next_token() {
			Token::EndOfFile => None,
			other => Some(other),
		}
	}
}

#[cfg(test)]
mod tests;
