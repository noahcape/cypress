use parsec::prelude::*;

#[test]
fn t_just() {
    let input = b"ABC".into_input();

    let parser = just('A');

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_then() {
    let input = b"ABC".into_input();

    let parser = just('A').then(just('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, (b'A', b'B')),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_or() {
    let input = b"B".into_input();

    let parser = just('A').or(just('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'B'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_map() {
    let input = b"A".into_input();

    let parser = just('A').map(|_| 1);

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 1),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_choice() {
    let input = b"A".into_input();

    let parser = choice!(just('C'), just('A'), just('C'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pbetween() {
    let input = "[A]".as_bytes().into_input();

    let parser = pbetween(just('['), just('A'), just(']'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pmany() {
    let input = b"ABCDE".into_input();

    let parser = pletter().many();

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, [b'A', b'B', b'C', b'D', b'E']),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_padded() {
    let input = b"   A   ".into_input();

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
fn t_ident() {
    let input = b"Noah Cape".into_input();

    let parser = pident("Noah")
        .padded_by(pws())
        .then(pident("Cape").padded_by(pws()));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => {
            assert_eq!(val, (String::from("Noah"), String::from("Cape")))
        }
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pstring() {
    let input = "\"This is a string\"".into_input();

    let parser = just('\"')
        .then(any().and(just('\"').not()).many())
        .then(just('\"'))
        .map(|((_, xs), _)| String::from_utf8(xs).unwrap());

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, String::from("This is a string")),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_recursive() {
    let input = b"(1+(2+3))".into_input();

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
