use crate::lexer::Lexer;
use crate::token::Token::*;
use anyhow::Result;

#[test]
fn test_lexer() -> Result<()> {
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
    assert_eq!(tests, lexer.lex()?);

    let input = r#"constructor var static a 1000"#;
    let tests = vec![
        Constructor,
        Var,
        Static,
        Ident("a".to_string()),
        IntConst("1000".to_string()),
    ];

    let mut lexer = Lexer::new(input);
    assert_eq!(tests, lexer.lex()?);

    let input = r#""abcdef""#;
    
    let tests = vec![
        StringConst("abcdef".to_string()),
    ];

    let mut lexer = Lexer::new(input);
    assert_eq!(tests, lexer.lex()?);

    Ok(())
}
