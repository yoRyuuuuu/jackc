use std::unimplemented;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{anyhow, Result};
#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
        };
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    pub fn parse_expr(&mut self) -> Result<Expression> {
        let left = self.parse_term()?;
        match self.cur_token {
            Token::Symbol(ref literal) => {
                let literal = literal.as_str();
                let op = match literal {
                    "+" => Operator::Plus,
                    "-" => Operator::Minus,
                    "*" => Operator::Aster,
                    "/" => Operator::Slash,
                    "&amp;" => Operator::And,
                    "|" => Operator::Or,
                    "&lt;" => Operator::LessThan,
                    "&gt;" => Operator::GreaterThan,
                    "=" => Operator::Assign,
                    _ => {
                        return Err(anyhow!(
                            "unexpected symbol {:?} in parse_expr",
                            self.cur_token
                        ))
                    }
                };
                self.next_token();
                let right = Box::new(self.parse_expr()?);
                Ok(Expression::Binary { left, op, right })
            }
            _ => Ok(Expression::Unary(left)),
        }
    }

    pub fn parse_term(&mut self) -> Result<Term> {
        let term = match self.cur_token {
            Token::IntConst(ref literal) => Term::IntConst(literal.to_owned()),
            Token::StringConst(ref literal) => Term::StringConst(literal.to_owned()),
            Token::True => Term::True,
            Token::False => Term::False,
            Token::Null => Term::Null,
            Token::This => Term::This,
            Token::Ident(ref string) => {
                unimplemented!();
            },
            _ => {
                return Err(anyhow!(
                    "unexpected token {:?} in parse_term",
                    self.cur_token
                ))
            }
        };
        self.next_token();
        Ok(term)
    }
}
