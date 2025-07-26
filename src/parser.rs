use std::{cell::RefCell, fmt::Display, marker::PhantomData, rc::Rc, sync::Arc};

pub mod core;
use core::*;

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

pub mod into;
use into::*;

pub mod utils;
use utils::*;

pub mod ident;

pub mod debug;
use debug::*;

pub fn just<K, T>(t: T) -> PSat<K>
where
    K: PartialEq + Display + Copy + 'static,
    T: IntoToken<K> + Copy,
{
    let token: K = t.into_token();
    psat(
        Box::new(move |i: K| i.eq(&token)),
        format!("Token at position doesn't match {token} (u8 rep)"),
    )
}

pub fn pnum<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(move |i: K| Into::<char>::into(i).is_ascii_digit()),
        "Token is not a number",
    )
}

pub fn pletter<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_alphabetic()),
        "Token is not a letter.",
    )
}

pub fn pws<K>() -> PSat<K>
where
    K: PartialEq + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_whitespace()),
        "Token is not whitespace.",
    )
}
