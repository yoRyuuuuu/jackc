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

    pub fn parse_statements(&mut self) -> Result<Vec<Statement>> {
        let mut stmts = vec![];

        loop {
            let stmt = match self.cur_token {
                Token::Let => self.parse_let_stmt()?,
                Token::If => self.parse_if_stmt()?,
                Token::While => {
                    unimplemented!()
                }
                Token::Do => {
                    unimplemented!()
                }
                Token::Return => {
                    unimplemented!()
                }
                _ => return Ok(stmts),
            };
            stmts.push(stmt);
        }
    }

    pub fn parse_let_stmt(&mut self) -> Result<Statement> {
        self.next_token();

        let name = match self.cur_token {
            Token::Ident(ref name) => name.to_owned(),
            _ => return Err(anyhow!("unexpected token {:?}", self.cur_token,)),
        };
        self.next_token();

        let index = match self.symbol_is("[") {
            Ok(_) => {
                self.next_token();
                let expr = self.parse_expr()?;
                self.symbol_is("]")?;
                self.next_token();
                Some(expr)
            }
            Err(_) => None,
        };

        self.symbol_is("=")?;
        self.next_token();

        let value = self.parse_expr()?;
        let stmt = Statement::LetStatement { name, index, value };

        self.symbol_is(";")?;
        self.next_token();

        Ok(stmt)
    }

    pub fn parse_if_stmt(&mut self) -> Result<Statement> {
        self.next_token(); // if

        self.symbol_is("(")?;
        self.next_token();

        let condition = self.parse_expr()?;

        self.symbol_is(")")?;
        self.next_token();

        self.symbol_is("{")?;
        self.next_token();

        let true_stmts = Box::new(self.parse_statements()?);

        self.symbol_is("}")?;
        self.next_token();

        let false_stmts = match self.cur_token {
            Token::Else => {
                self.next_token(); // else

                self.symbol_is("{")?;
                self.next_token();

                let false_stmts = self.parse_statements()?;

                self.symbol_is("}")?;
                self.next_token();

                Some(Box::new(false_stmts))
            }
            _ => None,
        };

        let stmt = Statement::IfStatement {
            condition,
            true_stmts,
            false_stmts,
        };

        Ok(stmt)
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
                "unexpected token {:?}. expected {:?} in symbol_is",
                self.cur_token,
                token
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;
    use anyhow::Result;

    #[test]
    fn test_lexer() -> Result<()> {
        use Token::*;
        let input = "=+-*~|;,.()";
        let tests = vec![
            Symbol("=".to_string()),
            Symbol("+".to_string()),
            Symbol("-".to_string()),
            Symbol("*".to_string()),
            Symbol("~".to_string()),
            Symbol("|".to_string()),
            Symbol(";".to_string()),
            Symbol(",".to_string()),
            Symbol(".".to_string()),
            Symbol("(".to_string()),
            Symbol(")".to_string()),
        ];

        let mut lexer = Lexer::new(input);
        assert_eq!(tests, lexer.lex());

        let input = r#"constructor var static a 1000"#;
        let tests = vec![
            Constructor,
            Var,
            Static,
            Ident("a".to_string()),
            IntConst("1000".to_string()),
        ];

        let mut lexer = Lexer::new(input);
        assert_eq!(tests, lexer.lex());

        let input = r#""abcdef""#;

        let tests = vec![StringConst("abcdef".to_string())];

        let mut lexer = Lexer::new(input);
        assert_eq!(tests, lexer.lex());

        Ok(())
    }

    #[test]
    fn test_parse_expr() -> Result<()> {
        use Token::*;
        let input = r#"1 + 2"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_expr()?;
        assert_eq!(
            Expression::Binary {
                left: Term::IntConst("1".to_string()),
                op: Operator::Plus,
                right: Box::new(Expression::Unary(Term::IntConst("2".to_string()))),
            },
            ast
        );

        let input = r#"1 + 2 - 3 * 4 / 5"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_expr()?;
        assert_eq!(
            Expression::Binary {
                left: Term::IntConst("1".to_string()),
                op: Operator::Plus,
                right: Box::new(Expression::Binary {
                    left: Term::IntConst("2".to_string()),
                    op: Operator::Minus,
                    right: Box::new(Expression::Binary {
                        left: Term::IntConst("3".to_string()),
                        op: Operator::Aster,
                        right: Box::new(Expression::Binary {
                            left: Term::IntConst("4".to_string()),
                            op: Operator::Slash,
                            right: Box::new(Expression::Unary(Term::IntConst("5".to_string())))
                        })
                    })
                })
            },
            ast
        );
        let input = r#"i = i + 1"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_expr()?;
        assert_eq!(
            Expression::Binary {
                left: Term::Var("i".to_string()),
                op: Operator::Assign,
                right: Box::new(Expression::Binary {
                    left: Term::Var("i".to_string()),
                    op: Operator::Plus,
                    right: Box::new(Expression::Unary(Term::IntConst("1".to_string()))),
                })
            },
            ast
        );

        let input = r#"sum = sum + a[i + 1] * a[i + 2]"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_expr()?;

        assert_eq!(
            Expression::Binary {
                left: Term::Var("sum".to_string()),
                op: Operator::Assign,
                right: Box::new(Expression::Binary {
                    left: Term::Var("sum".to_string()),
                    op: Operator::Plus,
                    right: Box::new(Expression::Binary {
                        left: Term::Array {
                            name: "a".to_string(),
                            index: Box::new(Expression::Binary {
                                left: Term::Var("i".to_string()),
                                op: Operator::Plus,
                                right: Box::new(Expression::Unary(Term::IntConst("1".to_string())))
                            })
                        },
                        op: Operator::Aster,
                        right: Box::new(Expression::Unary(Term::Array {
                            name: "a".to_string(),
                            index: Box::new(Expression::Binary {
                                left: Term::Var("i".to_string()),
                                op: Operator::Plus,
                                right: Box::new(Expression::Unary(Term::IntConst("2".to_string())))
                            })
                        }))
                    })
                })
            },
            ast
        );

        Ok(())
    }

    #[test]
    fn parse_term() -> Result<()> {
        let input = r#"a[i]"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_term()?;
        assert_eq!(
            Term::Array {
                name: "a".to_string(),
                index: Box::new(Expression::Unary(Term::Var("i".to_string())))
            },
            ast
        );
        let input = r#"(a[i])"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_term()?;
        assert_eq!(
            Term::Expr(Box::new(Expression::Unary(Term::Array {
                name: "a".to_string(),
                index: Box::new(Expression::Unary(Term::Var("i".to_string())))
            }))),
            ast
        );

        Ok(())
    }

    #[test]
    fn test_parse_let_stmt() -> Result<()> {
        let input = r#"let a = 10;"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_let_stmt()?;
        assert_eq!(
            Statement::LetStatement {
                name: "a".to_string(),
                index: None,
                value: Expression::Unary(Term::IntConst("10".to_string())),
            },
            ast
        );

        let input = r#"let a[10] = 10;"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_let_stmt()?;
        assert_eq!(
            Statement::LetStatement {
                name: "a".to_string(),
                index: Some(Expression::Unary(Term::IntConst("10".to_string()))),
                value: Expression::Unary(Term::IntConst("10".to_string())),
            },
            ast
        );

        Ok(())
    }

    #[test]
    fn test_if_stmt() -> Result<()> {
        let input = r#"if (false) {
    let s = "string constant";
    }
    else {
        let i = i * j;
    }"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_if_stmt()?;

        let true_stmts = vec![
            Statement::LetStatement {
            name: "s".to_string(),
            index: None,
            value: Expression::Unary(Term::StringConst("string cosstant".to_string())),
            }
        ];

        let false_stmts = vec![
            Statement::LetStatement {
                name: "i".to_string(),
                index: None,
                value: Expression::Binary {
                    left: Term::Var("i".to_string()),
                    op: Operator::Aster,
                    right: Box::new(Expression::Unary(Term::Var("j".to_string())))
                }
            }
        ];

        let stmt = Statement::IfStatement {
            condition: Expression::Unary(Term::True),
            true_stmts: Box::new(true_stmts),
            false_stmts: Some(Box::new(false_stmts)),
        };

        Ok(())
    }

    #[test]
    fn test_parse_statements() -> Result<()> {
        let input = r#"let a[10] = 10;
let a = 10;"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse_statements()?;
        let stmts = vec![
            Statement::LetStatement {
                name: "a".to_string(),
                index: Some(Expression::Unary(Term::IntConst("10".to_string()))),
                value: Expression::Unary(Term::IntConst("10".to_string())),
            },
            Statement::LetStatement {
                name: "a".to_string(),
                index: None,
                value: Expression::Unary(Term::IntConst("10".to_string())),
            },
        ];

        assert_eq!(stmts, ast);
        Ok(())
    }
}
