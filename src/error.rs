use std::borrow::Cow;
use std::fmt::Display;

use crate::parser::core::Span;
use crate::prelude::PInput;

/// A pattern representing one or more tokens or a string, used for descriptive error messages.
#[derive(Clone, Debug)]
pub enum TokenPattern<'a, K>
where
    K: Clone,
{
    /// A single token.
    Token(Cow<'a, K>),

    /// A sequence of tokens.
    Tokens(Cow<'a, [K]>),

    /// A human-readable string pattern.
    String(Cow<'a, str>),
}

/// A trait for custom error display formatting, allowing types like u8 that
/// need to be display in a special way to be diffentiated from things that implement display
///
/// **Note**: this currently requires users who want to parse enums to implement [`ErrorDisplay`] themselves.
pub trait ErrorDisplay {
    /// Formats the implementing type for display in errors.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result;
}

impl ErrorDisplay for &[u8] {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8(self.to_vec()).unwrap())
    }
}

impl ErrorDisplay for u8 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8(vec![*self]).unwrap())
    }
}

impl<'a, K> Display for TokenPattern<'a, K>
where
    K: Clone + ErrorDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenPattern::Token(Cow::Borrowed(token)) => token.fmt(f),
            TokenPattern::Token(Cow::Owned(token)) => token.fmt(f),
            TokenPattern::Tokens(Cow::Borrowed(tokens)) => {
                tokens.iter().try_for_each(|tok| tok.fmt(f))
            }
            TokenPattern::Tokens(Cow::Owned(tokens)) => {
                tokens.iter().try_for_each(|tok| tok.fmt(f))
            }
            TokenPattern::String(Cow::Borrowed(str)) => write!(f, "{str}"),
            TokenPattern::String(Cow::Owned(str)) => write!(f, "{str}"),
        }
    }
}

/// Represents the kind of error encountered during parsing.
#[derive(Clone, Debug)]
pub enum ErrorKind<'a, K>
where
    K: Clone,
{
    /// A custom string-based error.
    Custom(&'a str),

    /// Indicates that the input has been exhausted.
    EOF,

    /// Indicates that the parser encountered an unexpected token.
    Unexpected {
        /// List of expected patterns.
        expected: Vec<TokenPattern<'a, K>>,

        /// The actual token that was found.
        found: TokenPattern<'a, K>,
    },
}

impl<'a, K> Display for ErrorKind<'a, K>
where
    K: Clone + ErrorDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Custom(str) => write!(f, "{str}"),
            ErrorKind::EOF => write!(f, "No tokens to read: reached end of file"),
            ErrorKind::Unexpected { expected, found } => write!(
                f,
                "Expected: {} by found: \"{found}\"",
                expected
                    .iter()
                    .map(|tok_pat| format!("{tok_pat}"))
                    .collect::<Vec<String>>()
                    .join(","),
            ),
        }
    }
}

/// A parsing error that includes one or more kinds of errors, the input span where it occurred,
/// and the remaining parser state.
#[derive(Debug)]
pub struct Error<'a, K>
where
    K: PartialEq + Clone + 'a,
{
    /// The list of error kinds that occurred.
    pub kind: Vec<ErrorKind<'a, K>>,

    /// The span in the input where the error occurred.
    pub span: Span,

    /// The remaining input at the point of the error.
    pub state: PInput<'a, K>,
}

impl<'a, K> Error<'a, K>
where
    K: PartialEq + Clone + 'a,
{
    /// Creates a new `Error` with the given kind(s), input span, and parser state.
    pub fn new(kind: Vec<ErrorKind<'a, K>>, span: Span, state: PInput<'a, K>) -> Self {
        Error { kind, span, state }
    }
}

impl<'a, K> Display for Error<'a, K>
where
    K: PartialEq + Clone + ErrorDisplay + 'a,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.kind
                .iter()
                .map(|err| format!("{err}"))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}
