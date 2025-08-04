use std::{collections::HashMap, fmt::Display, fs::read_to_string};

use cypress::prelude::*;

/// A simple control flow language supporting variables which are ints or bools
/// and control flow of if and while. This language exihibts the use of `foldl`
/// to enforce precedence in [`expr_parser`].

#[derive(Debug, Clone)]
enum Statement {
    Let(String, Expr),
    While(Expr, Vec<Statement>),
    If(Expr, Vec<Statement>, Vec<Statement>),
    Print(Expr),
}

#[derive(Debug, Clone)]
enum Expr {
    Var(String),
    Num(i32),
    Bool(bool),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
    Mod,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum EvalToken {
    Num(i32),
    Bool(bool),
}

impl Display for EvalToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalToken::Num(n) => write!(f, "{}", n),
            EvalToken::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl EvalToken {
    fn is_num(&self) -> bool {
        matches!(self, EvalToken::Num(_))
    }

    fn get_num(&self) -> Option<i32> {
        match self {
            EvalToken::Num(n) => Some(*n),
            EvalToken::Bool(_) => None,
        }
    }
}

// Evaluation methods
impl BinOp {
    fn eval(&self, lhs: EvalToken, rhs: EvalToken) -> EvalToken {
        match self {
            BinOp::Add => {
                assert!(lhs.is_num() && rhs.is_num());
                EvalToken::Num(lhs.get_num().unwrap() + rhs.get_num().unwrap())
            }
            BinOp::Sub => {
                assert!(lhs.is_num() && rhs.is_num());
                EvalToken::Num(lhs.get_num().unwrap() - rhs.get_num().unwrap())
            }
            BinOp::Mul => {
                assert!(lhs.is_num() && rhs.is_num());
                EvalToken::Num(lhs.get_num().unwrap() * rhs.get_num().unwrap())
            }
            BinOp::Div => {
                assert!(lhs.is_num() && rhs.is_num());
                EvalToken::Num(lhs.get_num().unwrap() / rhs.get_num().unwrap())
            }
            BinOp::Mod => {
                assert!(lhs.is_num() && rhs.is_num());
                EvalToken::Num(lhs.get_num().unwrap() % rhs.get_num().unwrap())
            }
            BinOp::Eq => EvalToken::Bool(lhs == rhs),
            BinOp::Neq => EvalToken::Bool(lhs != rhs),
            BinOp::Gt => EvalToken::Bool(lhs > rhs),
            BinOp::Lt => EvalToken::Bool(lhs < rhs),
            BinOp::Gte => EvalToken::Bool(lhs >= rhs),
            BinOp::Lte => EvalToken::Bool(lhs <= rhs),
        }
    }
}

impl Expr {
    fn eval(&self, map: &HashMap<String, EvalToken>) -> EvalToken {
        match self {
            Expr::Var(name) => match map.get(name) {
                Some(e) => *e,
                None => panic!("No value associated with variable \"{name}\""),
            },
            Expr::Num(num) => EvalToken::Num(*num),
            Expr::Bool(b) => EvalToken::Bool(*b),
            Expr::BinOp(expr, bin_op, expr1) => bin_op.eval(expr.eval(map), expr1.eval(map)),
        }
    }
}

impl Statement {
    fn eval(&self, map: &mut HashMap<String, EvalToken>) -> EvalToken {
        match self {
            Statement::Let(name, expr) => {
                let eval_expr = expr.eval(map);
                let _ = map.insert(name.to_string(), eval_expr);
                eval_expr
            }
            Statement::While(expr, statements) => {
                loop {
                    if let EvalToken::Bool(true) = expr.eval(map) {
                        for statment in statements {
                            statment.eval(map);
                        }
                    } else {
                        break;
                    }
                }

                expr.eval(map)
            }
            Statement::If(expr, statements, statements1) => {
                if let EvalToken::Bool(true) = expr.eval(map) {
                    for statment in statements {
                        statment.eval(map);
                    }
                } else {
                    for statment in statements1 {
                        statment.eval(map);
                    }
                }

                expr.eval(map)
            }
            Statement::Print(expr) => {
                let eval_expr = expr.eval(map);
                println!("{}", eval_expr);
                eval_expr
            }
        }
    }
}

// Start of the parser
fn expr_parser<'a>() -> impl Parser<'a, u8, Expr> {
    let string = choice!(pletter(), just('_'))
        .many1()
        .map(|xs| Expr::Var(String::from_utf8(xs).unwrap()));

    let num = pnum()
        .many1()
        .map(|ns| Expr::Num(String::from_utf8(ns).unwrap().parse::<i32>().unwrap()));

    let bool_ =
        select! {(pident("true")) => Expr::Bool(true), (pident("false")) => Expr::Bool(false)};

    // Ensure that operations bind with correct precedence
    // Highest precedence
    let bc_rec = choice!(string.clone(), num.clone(), bool_.clone());

    // Second highest precedence
    let bin_op_3 = (bc_rec.clone().padded_by(pinlinews())).foldl(
        just('*')
            .into_(BinOp::Mul)
            .or(just('/').into_(BinOp::Div))
            .or(just('%').into_(BinOp::Mod))
            .then(bc_rec.padded_by(pinlinews()))
            .many(),
        |a, (op, b)| Expr::BinOp(Box::new(a), op, Box::new(b)),
    );

    // Third highest precedence
    let bin_op_2 = bin_op_3.clone().foldl(
        just('+')
            .into_(BinOp::Add)
            .or(just('-').into_(BinOp::Sub))
            .then(bin_op_3.padded_by(pinlinews()))
            .many(),
        |a, (op, b)| Expr::BinOp(Box::new(a), op, Box::new(b)),
    );

    // Lowest precedence
    let bin_op_1 = bin_op_2.clone().foldl(
        choice!(
            (sequence!('>' > '=')).into_(BinOp::Gte),
            (sequence!('<' > '=')).into_(BinOp::Lte),
            (sequence!('=' > '=')).into_(BinOp::Eq),
            (sequence!('!' > '=')).into_(BinOp::Neq),
            just('>').into_(BinOp::Gt),
            just('<').into_(BinOp::Lt),
        )
        .then(bin_op_2.padded_by(pinlinews()))
        .many(),
        |a, (op, b)| Expr::BinOp(Box::new(a), op, Box::new(b)),
    );

    choice!(bin_op_1, string, num, bool_)
}

fn statement_parser<'a>() -> impl Parser<'a, u8, Statement> {
    recursive(|stmt| {
        let let_ = (pident("let").padded_by(pinlinews()))
            .then(
                choice!(pletter(), just('_'))
                    .many1()
                    .map(|xs| String::from_utf8(xs).unwrap()),
            )
            .padded_by(pinlinews())
            .then(just('=').padded_by(pinlinews()).then(expr_parser()))
            .map(|((_, name), (_, expr))| Statement::Let(name, expr));

        let letless = choice!(pletter(), just('_'))
            .many1()
            .map(|xs| String::from_utf8(xs).unwrap())
            .then(just('=').padded_by(pinlinews()))
            .then(expr_parser())
            .map(|((name, _), expr)| Statement::Let(name, expr));

        let while_ = pident("while")
            .then(expr_parser().padded_by(pinlinews()))
            .then(pbetween(
                just('{').padded_by(pws()),
                (stmt.clone().padded_by(pinlinews()))
                    .delimited_by(just('\n').many())
                    .padded_by(pinlinews()),
                just('}').padded_by(pws()),
            ))
            .map(|((_, cond), stmts)| Statement::While(cond, stmts));

        let if_ = pident("if")
            .then(expr_parser().padded_by(pinlinews()))
            .then(pbetween(
                just('{').padded_by(pws()),
                (stmt.clone().padded_by(pinlinews()))
                    .delimited_by(just('\n').many())
                    .padded_by(pinlinews()),
                just('}').padded_by(pws()),
            ))
            .map(|((_, cond), stmts)| Statement::If(cond, stmts, vec![]));

        let if_else_ = pident("if")
            .then(expr_parser().padded_by(pinlinews()))
            .then(pbetween(
                just('{').padded_by(pws()),
                (stmt.clone().padded_by(pinlinews()))
                    .delimited_by(just('\n').many())
                    .padded_by(pinlinews()),
                just('}').padded_by(pws()),
            ))
            .then(pident("else"))
            .then(pbetween(
                just('{').padded_by(pws()),
                (stmt.clone().padded_by(pinlinews()))
                    .delimited_by(just('\n').many())
                    .padded_by(pinlinews()),
                just('}').padded_by(pws()),
            ))
            .map(|((((_, cond), if_stmts), _), else_stmts)| {
                Statement::If(cond, if_stmts, else_stmts)
            });

        let print_ = pident("print")
            .then(pbetween(just('('), expr_parser(), just(')')))
            .map(|(_, expr)| Statement::Print(expr));

        Box::new(choice!(let_, letless, while_, if_else_, if_, print_))
    })
}

fn parser<'a>() -> impl Parser<'a, u8, Vec<Statement>> {
    statement_parser()
        .delimited_by(just('\n').many())
        .until_end()
}

fn main() {
    let input =
        read_to_string("./examples/control.ctrl").expect("Failed to read file from examples dir.");

    match parser().parse(input.into_input()) {
        Ok(PSuccess { val, rest: _ }) => {
            let mut map: HashMap<String, EvalToken> = HashMap::new();
            // Evaluate each statment
            for statement in val {
                statement.eval(&mut map);
            }
        }
        Err(e) => println!("{}", e),
    }
}
