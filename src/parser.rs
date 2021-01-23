
use crate::ast::*;
use crate::token::Token;
use anyhow::{anyhow, Result};

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct Parser {
    tokens: Vec<Token>,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Self {
            tokens,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
        };
        parser.next_token().unwrap();
        parser.next_token().unwrap();
        parser
    }

    fn next_token(&mut self) -> Result<()> {
        self.cur_token = self.peek_token.to_owned();
        self.peek_token = self
            .tokens
            .iter()
            .next()
            .ok_or(anyhow!("no token"))?
            .to_owned();
        Ok(())
    }

}
