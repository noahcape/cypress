use crate::parser::utils::IntoToken;
use crate::prelude::PInput;
use std::fmt::Display;

use crate::parser::core::Span;

pub struct Underline<T>(pub T, pub Span);

impl<T> Display for Underline<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (start, stop) = self.1;
        let underline = Color::Red("-".repeat(stop - start));
        let carrot = Color::Red(">");

        write!(
            f,
            "{}\n{}{}{}",
            self.0,
            " ".repeat(start),
            carrot,
            underline
        )
    }
}

pub enum Color<K>
where
    K: Display,
{
    Red(K),
    Yellow(K),
    Plain(K),
}

impl<K> Display for Color<K>
where
    K: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::Red(str) => write!(f, "\x1b[31m{str}\x1b[0m"),
            Color::Yellow(str) => write!(f, "\x1b[33m{str}\x1b[0m"),
            Color::Plain(str) => write!(f, "{str}"),
        }
    }
}

pub enum ErrorKind<T, K>
where
    T: IntoToken<K>,
{
    Custom(String),
    EOF,
    Unexpected { expected: Vec<T>, found: T },
    PhantomMarker(K),
}

impl<T, K> Display for ErrorKind<T, K>
where
    T: IntoToken<K> + Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Custom(str) => write!(f, "{}", Color::Red(str)),
            ErrorKind::Unexpected { expected, found } => {
                let expected = expected
                    .iter()
                    .map(|exp| format!("{}", exp.into_token()))
                    .collect::<String>();
                write!(
                    f,
                    "\x1b[1mExpected:\x1b[0m {}\n\x1b[1mFound:\x1b[0m {}",
                    Color::Yellow(expected),
                    Color::Red(found.into_token())
                )
            }
            ErrorKind::EOF => write!(f, "{}", Color::Red("Reached end of file")),
            ErrorKind::PhantomMarker(_) => unimplemented!(),
        }
    }
}

pub struct Error<'a, K, T>
where
    K: PartialEq + Clone + 'a,
    T: IntoToken<K>,
{
    pub kind: ErrorKind<T, K>,
    pub span: Option<Span>,
    pub state: Option<PInput<'a, K>>,
}

impl<'a, T, K> Error<'a, K, T>
where
    K: PartialEq + Clone + 'a,
    T: IntoToken<K>,
{
    pub fn stateless(kind: ErrorKind<T, K>) -> Self {
        Error {
            kind,
            span: None,
            state: None,
        }
    }

    pub fn set_state(self, span: Span, state: PInput<'a, K>) -> Self {
        self.set_span(span).set_input(state)
    }

    fn set_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    fn set_input(mut self, state: PInput<'a, K>) -> Self {
        self.state = Some(state);
        self
    }
}

impl<'a, T, K> Display for Error<'a, K, T>
where
    K: PartialEq + Clone + Display + 'a,
    T: IntoToken<K> + Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
