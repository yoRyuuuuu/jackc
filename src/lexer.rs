use crate::token::{look_up_table, Token};
use anyhow::{anyhow, Result};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    ch: u8,
    pos: usize,
    consume_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            ch: 0,
            pos: 0,
            consume_pos: 0,
        };

        lexer.consume_char();
        lexer.skip_white();
        lexer
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while self.ch != 0 {
            let token = self.next_token();
            tokens.push(token);
        }

        tokens
    }

    pub fn next_token(&mut self) -> Token {
        use Token::*;
        let token = match self.ch {
            b'{' | b'}' | b'(' | b')' | b'[' | b']' | b'.' | b',' | b';' | b'+' | b'-' | b'*'
            | b'|' | b'=' | b'~' => {
                let token = Symbol((self.ch as char).to_string());
                self.consume_char();
                token
            }
            b'a'..=b'z' | b'A'..=b'Z' => {
                let literal = self
                    .consume_many(|ch| (b'a'..=b'z').contains(&ch) | (b'A'..=b'Z').contains(&ch));
                let token = look_up_table(literal);
                token
            }
            b'0'..=b'9' => {
                let number = self.consume_many(|ch| b"0123456789".contains(&ch));
                IntConst(number)
            }
            b'<' => {
                let token = Symbol("&lt;".to_string());
                self.consume_char();
                token
            }
            b'>' => {
                let token = Symbol("&gt;".to_string());
                self.consume_char();
                token
            }
            b'&' => {
                let token = Symbol("&amp;".to_string());
                self.consume_char();
                token
            }
            b'"' => {
                self.consume_char();
                let string = self.consume_many(|ch| ch != b'"');
                self.consume_char();
                StringConst(string)
            }
            b'/' => self.consume_comment(),
            _ => Eof,
        };

        self.skip_white();
        token
    }

    fn consume_many(&mut self, mut f: impl FnMut(u8) -> bool) -> String {
        let pos = self.pos;
        while f(self.ch) {
            self.consume_char();
        }
        self.input.get(pos..self.pos).unwrap().to_string()
    }

    fn consume_comment(&mut self) -> Token {
        use crate::token::Token::{Comment, Symbol};

        self.consume_char();
        match self.ch {
            b'/' => {
                self.consume_char();
                let string = self.consume_many(|ch| ch != b'\n').replace("\r", "");
                Comment(string)
            }
            b'*' => {
                self.consume_char();
                self.consume_char();
                let pos = self.pos;
                while !(self.ch == b'*' && self.peek_char() == b'/') {
                    self.consume_char();
                }
                let string = self
                    .input
                    .get(pos..self.pos)
                    .unwrap()
                    .to_string()
                    .replace("\r", "");

                self.consume_char();
                self.consume_char();

                Comment(string)
            }
            _ => Symbol("/".to_string()),
        }
    }

    fn skip_white(&mut self) {
        while let b' ' | b'\t' | b'\n' | b'\r' = self.ch {
            self.consume_char();
        }
    }

    fn peek_char(&mut self) -> u8 {
        if self.consume_pos < self.input.len() {
            self.input.as_bytes()[self.consume_pos]
        } else {
            0
        }
    }

    fn consume_char(&mut self) {
        if self.consume_pos < self.input.len() {
            self.ch = self.input.as_bytes()[self.consume_pos];
        } else {
            self.ch = 0;
        }
        self.pos = self.consume_pos;
        self.consume_pos += 1;
    }
}
