use std::fmt::Display;

/// Core parser components and traits.
pub mod core;
use core::*;

/// Recursive parser combinators allowing parsers to reference themselves,
/// useful for parsing recursive grammars.
pub mod recursive;

/// Parsers that handle padding around other parsers,
/// such as whitespace or delimiters before and after.
pub mod padded;

/// Negative lookahead parser combinators,
/// succeed if the inner parser fails at current input.
pub mod not;

/// Parsers handling delimiters, useful for parsing separated lists or tokens.
pub mod delim;

/// Parsers for matching zero or more repetitions of a pattern.
pub mod many;

/// Parsers allowing binding transformations of output.
pub mod bind;

/// Parsers that match tokens satisfying predicates.
/// Useful for matching specific token classes.
pub mod sat;
use sat::*;

/// Parsers sequencing multiple parsers in order.
pub mod seq;

/// Choice combinators allowing trying multiple parsers in sequence,
/// succeeding on the first successful parser.
///
/// This module exports the `choice!` macro for convenient chaining.
#[macro_use]
pub mod or;

/// Parsers that match between two delimiters.
/// Useful for bracketed or quoted sections.
pub mod between;

/// Conversions from input types (like slices, strings) into specified tokens.
pub mod into;

/// Utility functions and helpers used internally by parsers.
pub mod utils;
use utils::*;

/// Parsers specifically for identifiers.
pub mod ident;

/// Debugging parsers that print trace information for parser steps.
pub mod debug;

/// Parsers that combine multiple conditions (AND combinators).
pub mod and;

/// Creates a parser that matches exactly one token equal to `t`.
///
/// # Type Parameters
///
/// * `K` - The token type (must implement `PartialEq`, `Display`, `Copy`).
/// * `T` - The input type convertible into `K`.
///
/// # Returns
///
/// A `PSat` parser that accepts only the token `t`.
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

/// Creates a parser that accepts any numeric ASCII digit token.
///
/// # Type Parameters
///
/// * `K` - The token type convertible into `char`.
///
/// # Returns
///
/// A `PSat` parser that accepts tokens representing ASCII digits (0-9).
pub fn pnum<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(move |i: K| Into::<char>::into(i).is_ascii_digit()),
        "Token is not a number",
    )
}

/// Creates a parser that accepts any ASCII alphabetic letter token.
///
/// # Type Parameters
///
/// * `K` - The token type convertible into `char`.
///
/// # Returns
///
/// A `PSat` parser that accepts tokens representing ASCII letters (A-Z, a-z).
pub fn pletter<K>() -> PSat<K>
where
    K: PartialEq + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_alphabetic()),
        "Token is not a letter.",
    )
}

/// Creates a parser that accepts any ASCII whitespace token.
///
/// # Type Parameters
///
/// * `K` - The token type convertible into `char`.
///
/// # Returns
///
/// A `PSat` parser that accepts tokens representing ASCII whitespace.
pub fn pws<K>() -> PSat<K>
where
    K: PartialEq + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_whitespace()),
        "Token is not whitespace.",
    )
}

/// Creates a parser that accepts any token.
///
/// # Type Parameters
///
/// * `K` - The token type.
///
/// # Returns
///
/// A `PSat` parser that accepts any token unconditionally.
pub fn any<K>() -> PSat<K>
where
    K: PartialEq + Copy + 'static,
{
    psat(Box::new(|_| true), "")
}
