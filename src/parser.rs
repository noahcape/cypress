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

/// Parsers handling delimiters, useful for parsing delimited lists or tokens.
pub mod delim;

/// Parsers for matching between `at_least` and `at_most` repetitions of a pattern.
pub mod many;

/// Parsers allowing binding transformations of output.
pub mod bind;

/// Map result of parser with the input span it parsed
pub mod map_with_span;

/// Map result of parser if it is an error
pub mod map_error;

/// Parsers that match tokens satisfying predicates.
/// Useful for matching specific token classes.
pub mod sat;
use sat::*;

/// Parsing multiple parsers one after the other.
pub mod seq;

/// Parsing two parsers in sequences ignoring the result of the first
pub mod ignore_then;

/// Parsing two parsers in sequences ignore the result of the second
pub mod then_ignore;

/// Folding the result of parser into a single value
pub mod fold_left;

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

use crate::{error::TokenPattern, text::Char};

/// Parsers specifically for identifiers.
pub mod ident;

/// Debugging parsers that print trace information for parser steps.
pub mod debug;

/// Parsers that combine multiple conditions (AND combinators).
pub mod and;

/// Parser that asserts if an inner parser has consumed all input.
pub mod until_end;

#[macro_use]
pub mod macros;

/// Creates a parser that matches exactly one token equal to `t`.
///
/// # Type Parameters
///
/// * `K` - The token type (must implement `PartialEq`, `Display`).
/// * `T` - The input type convertible into `K`.
///
/// # Returns
///
/// A `PSat` parser that accepts only the token `t`.
pub fn just<'a, K, T>(t: T) -> PSat<'a, K>
where
    K: PartialEq + Display + Clone + 'static,
    T: IntoToken<K> + Clone,
{
    let token: K = t.clone().into_token();
    let token_c: K = t.into_token();
    psat(
        Box::new(move |i: &K| i.eq(&token)),
        vec![TokenPattern::Token(std::borrow::Cow::Owned(token_c))],
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
pub fn pnum<'a, K>() -> PSat<'a, K>
where
    K: PartialEq + Display + Char + Clone + 'static,
{
    psat(
        Box::new(move |i: &K| i.is_digit(10)),
        vec![TokenPattern::String(std::borrow::Cow::Borrowed(
            "Digit 0..9",
        ))],
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
pub fn pletter<'a, K>() -> PSat<'a, K>
where
    K: PartialEq + Char + Clone + 'static,
{
    psat(
        Box::new(|i: &K| match i.to_ascii() {
            Some(tok) => tok.is_ascii_alphabetic(),
            None => false,
        }),
        vec![TokenPattern::String(std::borrow::Cow::Borrowed(
            "Letter a..z",
        ))],
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
pub fn pws<'a, K>() -> PSat<'a, K>
where
    K: PartialEq + Char + Clone + 'static,
{
    psat(
        Box::new(|i: &K| i.is_whitespace()),
        vec![TokenPattern::String(std::borrow::Cow::Borrowed(
            "Whitespace token",
        ))],
    )
}

/// Creates a parser that accepts any inline ASCII whitespace token.
/// Meaning no newline tokens.
///
/// # Type Parameters
///
/// * `K` - The token type convertible into `char`.
///
/// # Returns
///
/// A `PSat` parser that accepts tokens representing inline ASCII whitespace.
pub fn pinlinews<'a, K>() -> PSat<'a, K>
where
    K: PartialEq + Char + Clone + 'static,
{
    psat(
        Box::new(|i: &K| i.is_inline_whitespace()),
        vec![TokenPattern::String(std::borrow::Cow::Borrowed(
            "Whitespace token",
        ))],
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
pub fn any<'a, K>() -> PSat<'a, K>
where
    K: PartialEq + Clone + 'static,
{
    psat(
        Box::new(|_: &_| true),
        vec![TokenPattern::String(std::borrow::Cow::Borrowed("Anything"))],
    )
}
