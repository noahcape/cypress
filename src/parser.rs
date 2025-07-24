use std::{cell::RefCell, fmt::Display, marker::PhantomData, rc::Rc, sync::Arc};

pub mod parser;
use parser::*;

pub mod recursive;
use recursive::*;

pub mod padded;
use padded::*;

pub mod not;
use not::*;

pub mod delim;
use delim::*;

pub mod many;
use many::*;

pub mod bind;
use bind::*;

pub mod sat;
use sat::*;

pub mod seq;
use seq::*;

#[macro_use]
pub mod or;
use or::*;

pub mod between;
use between::*;


pub fn pitem<K>(c: K) -> PSat<K>
where
    K: PartialEq + Display + Copy + 'static,
{
    psat(
        Box::new(move |i: K| i.eq(&c)),
        format!("Token doesn't match {}", c),
    )
}

pub fn pnum<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(move |i: K| Into::<char>::into(i).is_ascii_digit()),
        format!("Token is not a number"),
    )
}

pub fn pletter<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_alphabetic()),
        format!("Token is not a letter."),
    )
}

pub fn pws<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_whitespace()),
        format!("Token is not whitespace."),
    )
}

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
