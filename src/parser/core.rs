use crate::{
    error::{Error, ErrorDisplay},
    parser::{
        and::PAnd, between::PBetween, bind::PBind, debug::PDebug, delim::PDelim,
        fold_left::PFoldLeft, ignore_then::PIgnoreThen, into::PInto, many::PMany,
        map_error::PMapError, map_with_span::PMapWithSpan, not::PNot, or::POr, padded::PPaddedBy,
        seq::PSeq, then_ignore::PThenIgnore, until_end::PUntilEnd,
    },
    prelude::{
        debug, pand, pbetween, pbind, pdelim, pfoldl, pignore_then, pinto, pmany, pmap_error,
        pmap_with_span, pnot, por, ppadded, pseq, pthen_ignore, puntil_end,
    },
};

// temp: switch to Range<usize> at some point
pub type Span = (usize, usize);

/// Represents a parsing input state, including a slice of tokens and a position index.
///
/// This is the fundamental data structure passed to all parsers. It abstracts over token streams,
/// allowing your parser to work generically over any type `T` that behaves like a token.
///
/// # Type Parameters
/// - `'a`: The lifetime of the token slice
/// - `T`: The token type (e.g., `char`, `u8`, etc.)
#[derive(Clone, Debug)]
pub struct PInput<'a, T: PartialEq + Clone + 'a> {
    /// Slice of tokens being parsed
    pub tokens: &'a [T],

    /// Current index into the token stream
    pub loc: usize,
}

/// Represents a successful parse, including the parsed value and the remaining input.
///
/// This is the successful return type for all `ParserCore` implementations.
///
/// # Type Parameters
/// - `'a`: Lifetime of the input
/// - `T`: Token type
/// - `O`: Output type of the parser
#[derive(Clone)]
pub struct PSuccess<'a, T, O>
where
    T: PartialEq + Clone,
{
    /// The value produced by the parser
    pub val: O,

    /// The remaining input after parsing
    pub rest: PInput<'a, T>,
}

/// The core parsing trait implemented by all low-level parser combinators.
///
/// This trait defines the raw parsing interface. It should be implemented by all combinator types.
///
/// # Type Parameters
/// - `'a`: Lifetime of the input
/// - `K`: Token type
/// - `O`: Output type of the parser
pub trait ParserCore<'a, K, O>
where
    K: PartialEq + Clone + 'a,
{
    /// Attempt to parse from the given input, returning a result or failure.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, Error<'a, K>>;
}

/// High-level composable parser trait built on top of `ParserCore`.
///
/// This trait provides default combinators like `.map()`, `.then()`, `.or()`, etc.,
/// allowing ergonomic and chainable parser expressions.
///
/// # Type Parameters
/// - `'a`: Lifetime of the input
/// - `K`: Token type
/// - `O`: Output type of the parser
pub trait Parser<'a, K: PartialEq + Clone + 'a, O: Clone + 'a>:
    ParserCore<'a, K, O> + Sized + Clone
{
    /// Sequence two parsers and return a tuple of their results.
    fn then<P2, O2>(self, p2: P2) -> PSeq<Self, P2>
    where
        O2: Clone + 'a,
        P2: Parser<'a, K, O2>,
    {
        pseq(self, p2)
    }

    /// Try this parser or an alternative parser if this one fails.
    fn or<P2>(self, p2: P2) -> POr<Self, P2>
    where
        P2: Parser<'a, K, O>,
    {
        por(self, p2)
    }

    /// Apply a function to transform the output of the parser.
    fn map<O2, F>(self, f: F) -> PBind<Self, O, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: Clone + 'a,
    {
        pbind(self, f)
    }

    /// Apply this parser between two delimiters, discarding the delimiters' results.
    fn between<A, P1, P2>(self, l: P1, r: P2) -> PBetween<P1, Self, P2, A>
    where
        A: Clone + 'a,
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    /// Match this parser zero or more times and collect the results. This is the by
    /// default behavior of [`crate::parser::many::PMany`] yet one can specialize behavior
    /// with [`crate::parser::many::PMany::at_least`] and [`crate::parser::many::PMany::at_most`]
    fn many(self) -> PMany<Self> {
        pmany(self, 0, usize::MAX)
    }

    /// Match this parser one or more times and collect the results.
    fn many1(self) -> PMany<Self> {
        pmany(self, 1, usize::MAX)
    }

    /// Parse this parser zero or more times separated by a delimiter (but keep only the content values).
    /// By defualt allow trailing delimiters use [`crate::parser::delim::PDelim::no_trailing`],
    /// [`crate::parser::delim::PDelim::at_least`] and [`crate::parser::delim::PDelim::at_most`] to specialize this parser.
    fn delimited_by<PD, A>(self, delim: PD) -> PDelim<Self, PD, A>
    where
        A: Clone + 'a,
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim, 0, usize::MAX, true)
    }

    /// Parse this parser one or more times separated by a delimiter (but keep only the content values).
    fn delimited_by1<PD, A>(self, delim: PD) -> PDelim<Self, PD, A>
    where
        A: Clone + 'a,
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim, 1, usize::MAX, true)
    }

    /// Succeeds only if this parser fails, and vice versa. Produces no value.
    fn not(self) -> PNot<Self, O> {
        pnot(self)
    }

    /// Surround this parser with padding (like whitespace) and return the result.
    fn padded_by<PD, A>(self, pad: PD) -> PPaddedBy<Self, PD, A>
    where
        A: Clone + 'a,
        PD: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    /// Ignores the actual parsed value and replaces it with a given one.
    fn into_<Out>(self, out: Out) -> PInto<Self, O, Out, K>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    /// Debug tracing combinator to help inspect parsing behavior.
    fn debug(self, label: &'static str) -> PDebug<Self>
    where
        K: ErrorDisplay,
    {
        debug(self, label)
    }

    /// Apply another parser after this one, but return only the result of the first as well as the location after parsing.
    fn and<P2, A>(self, second: P2) -> PAnd<Self, P2, A>
    where
        A: Clone + 'a,
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }

    /// Succeeds if and only if the inner parser succeeds and consumes all input
    fn until_end(self) -> PUntilEnd<Self, K> {
        puntil_end(self)
    }

    /// Use `self` as `init` while folding left on `tail` using `f`
    fn foldl<TP, F, I, O2>(self, tail: TP, f: F) -> PFoldLeft<Self, TP, F, I, O2>
    where
        O2: Clone + 'a,
        I: IntoIterator<Item = O2> + Clone + 'a,
        TP: Parser<'a, K, I>,
        F: Fn(O, O2) -> O + Clone + 'a,
    {
        pfoldl(self, tail, f)
    }

    /// Parsers two parsers in sequence ignores the result of the first and returns the result of the second
    fn ignore_then<P2, OO>(self, then: P2) -> PIgnoreThen<Self, P2, O>
    where
        OO: Clone + 'a,
        P2: Parser<'a, K, OO>,
    {
        pignore_then(self, then)
    }

    /// Parsers two parsers in sequence returns the result of the first and ignores the result of the second
    fn then_ignore<P2, OI>(self, ignore: P2) -> PThenIgnore<Self, P2, OI>
    where
        OI: Clone + 'a,
        P2: Parser<'a, K, OI>,
    {
        pthen_ignore(self, ignore)
    }

    /// Map the result of the parser through a function with the span which the parser was successful parsing
    fn map_with_span<F, O2>(self, f: F) -> PMapWithSpan<Self, O, O2>
    where
        F: Fn(O, Span) -> O2 + 'static,
        O2: Clone + 'a,
    {
        pmap_with_span(self, f)
    }

    /// Map an error of `self` with `f` if it fails else result the result of `self`
    fn map_error<F>(self, f: F) -> PMapError<'a, K, Self>
    where
        F: Fn(Error<'a, K>) -> Error<'a, K> + 'static,
    {
        pmap_error(self, f)
    }
}
