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
