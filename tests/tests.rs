use parsec::{
    choice,
    parser::{between::pbetween, core::*, padded::ppadded, recursive::recursive, *},
};

#[test]
fn t_just() {
    let input = prepare(b"ABC");

    let parser = just('A');

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_then() {
    let input = prepare(b"ABC");

    let parser = just('A').then(just('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, (b'A', b'B')),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_or() {
    let input = prepare(b"B");

    let parser = just('A').or(just('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'B'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_map() {
    let input = prepare(b"A");

    let parser = just('A').map(|_| 1);

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 1),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_choice() {
    let input = prepare(b"A");

    let parser = choice!(just('C'), just('A'), just('C'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pbetween() {
    let input = prepare("[A]".as_bytes());

    let parser = pbetween(just('['), just('A'), just(']'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pmany() {
    let input = prepare("ABCDE".as_bytes());

    let parser = pletter().many();

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, [b'A', b'B', b'C', b'D', b'E']),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_padded() {
    let input = prepare("   A   ".as_bytes());

    let parser = ppadded(just('A'), pws::<u8>());

    match parser.parse(input) {
        Ok(PSuccess { val, rest }) => {
            assert_eq!(val, b'A');
            assert_eq!(rest.loc, rest.tokens.len())
        }
        Err(_) => assert!(false),
    }
}

#[test]
fn t_recursive() {
    let input = prepare("(1+(2+3))".as_bytes());

    #[derive(Debug, PartialEq)]
    enum AST {
        Num(u32),
        Expr(Box<AST>, Box<AST>),
    }

    let parser = recursive(|expr| {
        Box::new(choice!(
            pnum().map(|a: u8| AST::Num((a - b'0').into())),
            just('(')
                .then(expr.clone())
                .then(just('+').then(expr))
                .then(just(')'))
                .map(|(((_, lhs), (_, rhs)), _)| AST::Expr(Box::new(lhs), Box::new(rhs)))
        ))
    });

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(
            val,
            AST::Expr(
                Box::new(AST::Num(1)),
                Box::new(AST::Expr(Box::new(AST::Num(2)), Box::new(AST::Num(3))))
            )
        ),
        Err(_) => assert!(false),
    }
}
