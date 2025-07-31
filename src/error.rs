use crate::prelude::PInput;
use crate::{parser::utils::IntoToken, text::Char};
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

pub enum Style<K>
where
    K: Display,
{
    Bold(K),
    Plain(K),
}

impl<K> Display for Style<K>
where
    K: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Style::Bold(i) => write!(f, "\x1b[1m{i}\x1b[0m"),
            Style::Plain(i) => write!(f, "{i}"),
        }
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
            Color::Red(i) => write!(f, "\x1b[31m{i}\x1b[0m"),
            Color::Yellow(i) => write!(f, "\x1b[33m{i}\x1b[0m"),
            Color::Plain(i) => write!(f, "{i}"),
        }
    }
}

pub enum ErrorKind<D>
where
    D: Display,
{
    Custom(String),
    EOF,
    Unexpected { expected: Vec<D>, found: D },
}

impl<D> Display for ErrorKind<D>
where
    D: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Custom(str) => write!(f, "{}", Color::Red(str)),
            ErrorKind::Unexpected { expected, found } => {
                let expected = expected
                    .iter()
                    .map(|exp| format!("{exp}"))
                    .collect::<String>();
                write!(
                    f,
                    "{} {}\n{} {}",
                    Style::Bold("Expected:"),
                    Style::Bold("Found:"),
                    Color::Yellow(expected),
                    Color::Red(found)
                )
            }
            ErrorKind::EOF => write!(f, "{}", Color::Red("Reached end of file")),
        }
    }
}

pub struct Error<'a, K, D>
where
    K: PartialEq + Clone + 'a,
    D: Display,
{
    pub kind: ErrorKind<D>,
    pub span: Span,
    pub state: PInput<'a, K>,
}

impl<'a, K, D> Error<'a, K, D>
where
    K: PartialEq + Clone + Char + 'a,
    D: Display,
{
    pub fn new(kind: ErrorKind<D>, span: Span, state: PInput<'a, K>) -> Self {
        Error { kind, span, state }
    }

    pub fn fmt_state(&self) -> String {
        let ((line_start, line_stop), _) = self.state.span_snapshot(self.span);

        format!(
            "{} {}-{}\n\nSource:\n{}",
            Style::Bold(Color::Red("Error at:")),
            Style::Bold(line_start),
            Style::Bold(line_stop),
            self.state.fmt_tokens()
        )
    }
}

impl<'a, T, K> Display for Error<'a, K, T>
where
    K: PartialEq + Clone + Display + Char + 'a,
    T: IntoToken<K> + Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n\n{}", self.fmt_state(), self.kind)
    }
}

impl<'a, T> PInput<'a, T>
where
    T: PartialEq + Clone + Char + 'a,
{
    fn span_snapshot(&self, span: Span) -> (Span, usize) {
        let (expected_start, expected_stop) = span;

        let mut start = 0;
        let mut stop = 0;
        let mut tick_offset = 0;
        let mut offset = 0;
        let mut line_num = 0;

        for (i, tok) in self.tokens.iter().enumerate() {
            if tok.is_newline() {
                line_num += 1;
                tick_offset = i;
            }

            if i == expected_start {
                start = line_num;
                offset = tick_offset;
            }

            if i == expected_stop {
                stop = line_num;
                break;
            }
        }

        ((start, stop), offset)
    }

    fn fmt_tokens(&self) -> String {
        String::from_utf8(
            self.tokens
                .iter()
                .map(|tok| tok.to_ascii().unwrap())
                .collect(),
        )
        .unwrap()
    }
}
