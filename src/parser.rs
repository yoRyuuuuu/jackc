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
                    _ => return Ok(Expression::Unary(left)),
                };

                self.next_token();
                let right = Box::new(self.parse_expr()?);
                Ok(Expression::Binary { left, op, right })
            }
            _ => Ok(Expression::Unary(left)),
        }
    }

    pub fn parse_term(&mut self) -> Result<Term> {
        let term = match self.cur_token.clone() {
            Token::IntConst(ref literal) => Term::IntConst(literal.to_owned()),
            Token::StringConst(ref literal) => Term::StringConst(literal.to_owned()),
            Token::True => Term::True,
            Token::False => Term::False,
            Token::Null => Term::Null,
            Token::This => Term::This,
            Token::Ident(name) => {
                self.next_token();
                match self.cur_token {
                    Token::Symbol(ref literal) => {
                        let literal = literal.as_str();
                        match literal {
                            "[" => {
                                self.next_token();
                                let expr = self.parse_expr()?;
                                self.symbol_is("]")?;
                                Term::Array {
                                    name: name.to_owned(),
                                    index: Box::new(expr),
                                }
                            }
                            _ => return Ok(Term::Var(name)),
                        }
                    }
                    _ => return Ok(Term::Var(name)),
                }
            }
            Token::Symbol(_) => {
                self.symbol_is("(")?;
                self.next_token();
                let expr = self.parse_expr()?;
                self.symbol_is(")")?;
                self.next_token();
                return Ok(Term::Expr(Box::new(expr)));
            }
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

    pub fn symbol_is(&self, literal: &str) -> Result<()> {
        let token = Token::Symbol(literal.to_owned());
        if self.cur_token != token {
            return Err(anyhow!(
                "unexpected symbol {:?}. expected {:?}",
                self.cur_token,
                token
            ));
        }
        Ok(())
    }
}
