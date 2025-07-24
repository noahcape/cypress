use std::{cell::RefCell, fmt::Display, marker::PhantomData, rc::Rc, sync::Arc};

pub mod parser;
use parser::*;

pub mod recursive;

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
        format!("Token doesn't match {c}"),
    )
}

pub fn pnum<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(move |i: K| Into::<char>::into(i).is_ascii_digit()),
        "Token is not a number".to_string(),
    )
}

pub fn pletter<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_alphabetic()),
        "Token is not a letter.".to_string(),
    )
}

pub fn pws<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_whitespace()),
        "Token is not whitespace.".to_string(),
    )
}
