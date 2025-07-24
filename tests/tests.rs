use parsec::{
    choice,
    parser::{between::pbetween, padded::ppadded, parser::*, recursive::recursive, *},
};

#[test]
fn t_pitem() {
    let input = prepare("ABC".chars());

    let parser = pitem('A');

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_then() {
    let input = prepare("ABC".chars());

    let parser = pitem('A').then(pitem('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, ('A', 'B')),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_or() {
    let input = prepare("B".chars());

    let parser = pitem('A').or(pitem('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'B'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_map() {
    let input = prepare("A".chars());

    let parser = pitem('A').map(|_| 1);

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 1),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_choice() {
    let input = prepare("A".chars());

    let parser = choice!(pitem('C'), pitem('A'), pitem('C'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pbetween() {
    let input = prepare("[A]".chars());

    let parser = pbetween(pitem('['), pitem('A'), pitem(']'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pmany() {
    let input = prepare("ABCDE".chars());

    let parser = pletter().many();

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, ['A', 'B', 'C', 'D', 'E']),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_padded() {
    let input = prepare("   A   ".chars());

    let parser = ppadded(pitem('A'), pws::<char>());

    match parser.parse(input) {
        Ok(PSuccess { val, rest }) => {
            assert_eq!(val, 'A');
            assert_eq!(rest.tokens.count(), 0)
        }
        Err(_) => assert!(false),
    }
}

#[test]
fn t_recursive() {
    let input = prepare("(1+(2+3))".chars());

    #[derive(Debug, PartialEq)]
    enum AST {
        Num(u32),
        Expr(Box<AST>, Box<AST>),
    }

    let parser = recursive(|expr| {
        Box::new(choice!(
            pnum().map(|a: char| AST::Num(a.to_digit(10).unwrap())),
            pitem('(')
                .then(expr.clone())
                .then(pitem('+').then(expr))
                .then(pitem(')'))
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
