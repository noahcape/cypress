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
                assert!(
                    lhs.is_num() && rhs.is_num(),
                    "Both arguments must be numbers"
                );
                EvalToken::Num(lhs.get_num().unwrap() + rhs.get_num().unwrap())
            }
            BinOp::Sub => {
                assert!(
                    lhs.is_num() && rhs.is_num(),
                    "Both arguments must be numbers"
                );
                EvalToken::Num(lhs.get_num().unwrap() - rhs.get_num().unwrap())
            }
            BinOp::Mul => {
                assert!(
                    lhs.is_num() && rhs.is_num(),
                    "Both arguments must be numbers"
                );
                EvalToken::Num(lhs.get_num().unwrap() * rhs.get_num().unwrap())
            }
            BinOp::Div => {
                assert!(
                    lhs.is_num() && rhs.is_num(),
                    "Both arguments must be numbers"
                );
                EvalToken::Num(lhs.get_num().unwrap() / rhs.get_num().unwrap())
            }
            BinOp::Mod => {
                assert!(
                    lhs.is_num() && rhs.is_num(),
                    "Both arguments must be numbers"
                );
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
    fn eval(&self, map: &mut HashMap<String, EvalToken>) {
        match self {
            Statement::Let(name, expr) => {
                let _ = map.insert(name.to_string(), expr.eval(map));
            }
            Statement::While(expr, statements) => loop {
                if let EvalToken::Bool(true) = expr.eval(map) {
                    for statment in statements {
                        statment.eval(map);
                    }
                } else {
                    break;
                }
            },
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
            }
            Statement::Print(expr) => {
                println!("{}", expr.eval(map));
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

    let bool_ = select! {
        (pident("true")) => Expr::Bool(true),
        (pident("false")) => Expr::Bool(false)
    };

    let atom = choice!(string.clone(), num.clone(), bool_.clone()).padded_by(pinlinews());

    // User precedence macro to simple define precedence levels
    let bin_op = precedence! {
        // highest precedece
        atom,
        {
            choice!(
                select! { '*' => BinOp::Mul },
                select! { '/' => BinOp::Div },
                select! { '%' => BinOp::Mod },
            )
            =>
            |a, (op, b)| Expr::BinOp(Box::new(a), op, Box::new(b))
        },
        {
            choice!(
                select! { '+' => BinOp::Add },
                select! { '-' => BinOp::Sub },
            )
            =>
            |a, (op, b)| Expr::BinOp(Box::new(a), op, Box::new(b))
        },
        // lowest precedence
        {
            choice!(
                select! { (sequence!('>' > '=')) => BinOp::Gte },
                select! { (sequence!('<' > '=')) => BinOp::Lte },
                select! { (sequence!('=' > '=')) => BinOp::Eq },
                select! { (sequence!('!' > '=')) => BinOp::Neq },
                select! { '<' => BinOp::Lt },
                select! { '>' => BinOp::Gt },
            )
            =>
            |a, (op, b)| Expr::BinOp(Box::new(a), op, Box::new(b))
        }
    };

    choice!(bin_op, string, num, bool_)
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
