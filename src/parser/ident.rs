use crate::{
    error::{Error, ErrorKind, TokenPattern},
    parser::*,
};

/// A parser that matches a fixed identifier string in the input.
///
/// `PIdent` attempts to parse a specific string slice (`ident`) from the input tokens.
/// The input tokens are converted into characters to compare with the target identifier.
///
/// # Lifetime
///
/// * `'a` - Lifetime of the input string slice `ident`.
#[derive(Clone)]
pub struct PIdent<'a> {
    /// The string slice representing the identifier to match.
    ident: &'a str,
}

/// Creates a new `PIdent` parser that matches the exact string `ident`.
/// Note that this is only implemented for input types [char] or [u8] since
/// these are the logical types to compare against a [str].
///
/// # Arguments
///
/// * `ident` - The identifier string to match against the input.
///
/// # Returns
///
/// A `PIdent` instance that implements `ParserCore` and attempts to match
/// the given identifier string from the input tokens.
pub fn pident<'a>(ident: &'a str) -> PIdent<'a> {
    PIdent { ident }
}

impl<'a> ParserCore<'a, u8, &'a str> for PIdent<'a> {
    /// Attempts to parse the identifier string from the input of u8.
    ///
    /// This method extracts a [u8] from the input tokens of length equal to the
    /// identifier's length, and directly compares to the ident `.as_bytes()`
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input containing tokens and current location.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with the matched identifier string and updated input location
    ///   if the input matches the identifier exactly.
    /// * `Err(Error)` containing an error message, the span of the failed match,
    ///   and the input position after the attempted parse if the match fails.
    fn parse(&self, i: PInput<'a, u8>) -> Result<PSuccess<'a, u8, &'a str>, Error<'a, u8>> {
        let ident_len = self.ident.len();

        if i.tokens.len() < i.loc + ident_len {
            return Err(Error {
                kind: vec![ErrorKind::Unexpected {
                    expected: vec![TokenPattern::String(std::borrow::Cow::Borrowed(self.ident))],
                    found: TokenPattern::String(std::borrow::Cow::Borrowed("Not enough tokens")),
                }],
                span: (i.loc, i.tokens.len()),
                state: PInput {
                    tokens: i.tokens,
                    loc: i.loc,
                },
            });
        }

        // Compare input tokens to identified
        if &i.tokens[i.loc..i.loc + ident_len] == self.ident.as_bytes() {
            // Successful parse: return matched string and updated location
            Ok(PSuccess {
                val: self.ident,
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc + ident_len,
                },
            })
        } else {
            // Failed parse: return error message, span, and updated input location
            Err(Error {
                kind: vec![ErrorKind::Unexpected {
                    expected: vec![TokenPattern::String(std::borrow::Cow::Borrowed(self.ident))],
                    found: TokenPattern::Tokens(std::borrow::Cow::Borrowed(
                        &i.tokens[i.loc..i.loc + ident_len],
                    )),
                }],
                span: (i.loc, i.loc + ident_len),
                state: PInput {
                    tokens: i.tokens,
                    loc: i.loc,
                },
            })
        }
    }
}

impl<'a> ParserCore<'a, char, &'a str> for PIdent<'a> {
    /// Attempts to parse the identifier string from the input of char.
    ///
    /// This method extracts a slice from the input [char]s of length equal to the
    /// identifier's length, then converts the ident into chars and compares each
    /// element.
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input containing tokens and current location.
    ///
    /// # Returns
    ///
    /// * `Ok(PSuccess)` with the matched identifier string and updated input location
    ///   if the input matches the identifier exactly.
    /// * `Err(Error)` containing an error message, the span of the failed match,
    ///   and the input position after the attempted parse if the match fails.
    fn parse(&self, i: PInput<'a, char>) -> Result<PSuccess<'a, char, &'a str>, Error<'a, char>> {
        let ident_len = self.ident.len();

        if i.tokens.len() < i.loc + ident_len {
            return Err(Error {
                kind: vec![ErrorKind::Unexpected {
                    expected: vec![TokenPattern::String(std::borrow::Cow::Borrowed(self.ident))],
                    found: TokenPattern::String(std::borrow::Cow::Borrowed("Not enough tokens")),
                }],
                span: (i.loc, i.tokens.len()),
                state: PInput {
                    tokens: i.tokens,
                    loc: i.loc,
                },
            });
        }

        // Compare input tokens to identified
        if self
            .ident
            .chars()
            .zip(i.tokens[i.loc..i.loc + ident_len].iter())
            .all(|(a, b)| a == *b)
        {
            // Successful parse: return matched string and updated location
            Ok(PSuccess {
                val: self.ident,
                rest: PInput {
                    tokens: i.tokens,
                    loc: i.loc + ident_len,
                },
            })
        } else {
            // Failed parse: return error message, span, and updated input location
            Err(Error {
                kind: vec![ErrorKind::Unexpected {
                    expected: vec![TokenPattern::String(std::borrow::Cow::Borrowed(self.ident))],
                    found: TokenPattern::Tokens(std::borrow::Cow::Borrowed(
                        &i.tokens[i.loc..i.loc + ident_len],
                    )),
                }],
                span: (i.loc, i.loc + ident_len),
                state: PInput {
                    tokens: i.tokens,
                    loc: i.loc,
                },
            })
        }
    }
}

impl<'a> Parser<'a, u8, &'a str> for PIdent<'a> {}

impl<'a> Parser<'a, char, &'a str> for PIdent<'a> {}
