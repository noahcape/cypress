use std::{collections::HashMap, fmt::Display};

use cypress::prelude::*;

/// foo is a play language with assignment and printing.
/// There are two types of expressions `print <expr>` and `<ident> = <expr>`.
/// Each expression returns a value, `print` returns the value that is printed
/// and assignment returns the value that was assigned. Thus a valid sentence may
/// be `print x = y = 4` how this would expected is: first y gets 4, the x gets 4
/// then the program finishes with printing 4. Lines are delimitted by a newline
/// and executed linearly in sequence.
///
/// This language cannot do much but it can help give a hint of how to write a
/// parser using cypress.

#[derive(PartialEq, Clone, Debug, Eq, Hash)]
enum Expr {
    Str(String),
    Num(i32),
    Var(String),
    Assignment(String, Box<Expr>),
    Print(Box<Expr>),
    Seq(Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Str(str) => write!(f, "\"{}\"", str),
            Expr::Num(num) => write!(f, "{}", num),
            Expr::Var(var) => write!(f, "{}", var),
            Expr::Assignment(name, expr) => write!(f, "{} = {}", name, expr),
            Expr::Print(expr) => write!(f, "{}", expr),
            Expr::Seq(exprs) => write!(
                f,
                "{}\n",
                exprs
                    .iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<String>()
            ),
        }
    }
}

fn num<'a>() -> impl Parser<'a, u8, i32> {
    pnum()
        .many()
        .map(|xs| String::from_utf8(xs).unwrap().parse::<i32>().unwrap())
}

fn str_<'a>() -> impl Parser<'a, u8, String> {
    pbetween(just('\"'), pletter().many(), just('\"')).map(|val| String::from_utf8(val).unwrap())
}

fn ident<'a>() -> impl Parser<'a, u8, String> {
    let start = choice!(pletter(), just('_'));
    let valid_idents = choice!(pnum(), just('_'), pletter());

    let ident = start.then(valid_idents.many()).map(|(s, mut rest)| {
        let mut all = vec![s];
        all.append(&mut rest);
        all
    });

    ident.map(|val| String::from_utf8(val).unwrap())
}

/// Parsing a variable assignment, note the use of `sequence!`
/// a macro to support applying multiple parsers in sequence. See `print`
/// below for an example of doing a similar parse without this macro.
fn var_assignment<'a>(expr: impl Parser<'a, u8, Expr>) -> impl Parser<'a, u8, Expr> {
    sequence!(
        (ident())                       >
        (just('=').padded_by(pws()))    >
        expr                            =>
        |(name, (_, val))| Expr::Assignment(name, Box::new(val))
    )
}

/// An example of applying two parsers in sequence without `sequence!` macro.
fn print<'a>(expr: impl Parser<'a, u8, Expr>) -> impl Parser<'a, u8, Expr> {
    let print_ident = pident("print");

    print_ident
        .padded_by(pws())
        .then(expr)
        .map(|(_, val)| Expr::Print(Box::new(val)))
}

fn expr<'a>() -> impl Parser<'a, u8, Expr> {
    recursive(|expr| {
        Box::new(
            choice!(
                var_assignment(expr.clone()),
                print(expr),
                ident().map(Expr::Var),
                str_().map(Expr::Str),
                num().map(Expr::Num)
            )
            .map(|expr| expr),
        )
    })
}

fn parser<'a>() -> impl Parser<'a, u8, Expr> {
    (expr().delimited_by(just('\n'))).until_end().map(Expr::Seq)
}

fn eval<'a>(expr: Expr, var_map: &mut HashMap<String, Expr>) -> Result<Expr, String> {
    match expr {
        Expr::Str(_) | Expr::Num(_) => Ok(expr),
        Expr::Var(name) => match var_map.get(&name) {
            Some(val) => eval(val.clone(), var_map),
            None => Err(format!("No value associated with key: \"{}\"", name)),
        },
        Expr::Assignment(name, expr) => {
            let eval_expr = eval(*expr, var_map)?;
            var_map.insert(name, eval_expr.clone());
            Ok(eval_expr)
        }
        Expr::Print(expr) => {
            let eval_expr = eval(*expr, var_map)?;
            println!("{}", eval_expr);
            Ok(eval_expr)
        }
        Expr::Seq(exprs) => {
            let mut res = vec![];
            for expr in exprs {
                let expr_res = eval(expr.clone(), var_map);
                match expr_res {
                    Ok(eval_expr) => res.push(eval_expr),
                    Err(err) => return Err(err),
                }
            }

            Ok(Expr::Seq(res))
        }
    }
}

fn main() {
    let input = b"y = print z = 4
x = print y
y = 10
print y
print x
print z";

    match parser().parse(input.into_input()) {
        Ok(PSuccess { val, rest: _ }) => {
            print!("{:?}", val);
            let res = eval(val, &mut HashMap::new());
            match res {
                Ok(ret) => println!("{:?}", ret),
                Err(err) => println!("Evaluation Error: {}", err),
            }
        }
        Err(e) => {
            println!("{}", e)
        }
    }
}
