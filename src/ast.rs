#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassVarDec {
    pub fld_typ: FieldType,
    pub typ: Type,
    pub names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldType {
    Static,
    Field,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    Class(String),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubroutineDec {
    pub sub_typ: SubroutineType,
    pub ret_typ: Type,
    pub name: String,
    pub params: Vec<ParameterDec>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SubroutineType {
    Constructor,
    Function,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterDec {
    pub typ: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubroutineBody {
    pub vars: Option<VarDec>,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDec {
    pub typ: Type,
    pub names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStatement {
        name: String,
        index: Option<Expression>,
        value: Expression,
    },
    IfStatement {
        condition: Expression,
        true_stmts: Box<Vec<Statement>>,
        false_stmts: Option<Box<Vec<Statement>>>,
    },
    WhileStatement {
        condition: Expression,
        stmts: Box<Vec<Statement>>,
    },
    DoStatement(SubroutineCall),
    ReturnStatement(Option<Expression>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum SubroutineCall {
    FuncCall {
        sub_name: String,
        expr_list: Option<Vec<Expression>>,
    },
    MethodCall {
        name: String,
        sub_name: String,
        expr_list: Option<Vec<Expression>>
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Unary(Term),
    Binary {
        left: Term,
        op: Operator,
        right: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    IntConst(String),
    StringConst(String),
    True,
    False,
    Null,
    This,
    Var(String),
    Array {
        name: String,
        index: Box<Expression>,
    },
    FunctionCall {
        name: String,
        exprs: Option<Vec<Expression>>,
    },
    MethodCall {
        name: String,
        sub_name: String,
        exprs: Option<Vec<Expression>>,
    },
    Expr(Box<Expression>),
    Unary {
        op: UnaryOp,
        term: Box<Term>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Minus,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Aster,
    Slash,
    And,
    Or,
    GreaterThan,
    LessThan,
    Assign,
}
