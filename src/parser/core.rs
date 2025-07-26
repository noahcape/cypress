/// Represents a parsing input state, including a slice of tokens and a position index.
///
/// This is the fundamental data structure passed to all parsers. It abstracts over token streams,
/// allowing your parser to work generically over any type `T` that behaves like a token.
///
/// # Type Parameters
/// - `'a`: The lifetime of the token slice
/// - `T`: The token type (e.g., `char`, `u8`, etc.)
#[derive(Clone, Debug)]
pub struct PInput<'a, T: PartialEq + Copy + Clone + 'a> {
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
    T: PartialEq + Copy + Clone,
{
    /// The value produced by the parser
    pub val: O,

    /// The remaining input after parsing
    pub rest: PInput<'a, T>,
}

/// Represents a failed parse, including an error message, the span of failure, and the remaining input.
///
/// Used to propagate errors in a structured way with location information.
///
/// # Type Parameters
/// - `'a`: Lifetime of the input
/// - `T`: Token type
pub struct PFail<'a, T>
where
    T: PartialEq + Copy + Clone,
{
    /// Human-readable error message
    pub error: String,

    /// Span (start and end indices) where the failure occurred
    pub span: (usize, usize),

    /// The input state at the point of failure
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
    K: PartialEq + Copy + Clone + 'a,
{
    /// Attempt to parse from the given input, returning a result or failure.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>>;
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
pub trait Parser<'a, K: PartialEq + Copy + Clone + 'a, O>: ParserCore<'a, K, O> + Sized {
    /// Sequence two parsers and return a tuple of their results.
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a;

    /// Try this parser or an alternative parser if this one fails.
    fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
    where
        T: Parser<'a, K, O>;

    /// Apply a function to transform the output of the parser.
    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a;

    /// Apply this parser between two delimiters, discarding the delimiters' results.
    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>;

    /// Match this parser zero or more times and collect the results.
    fn many(self) -> impl Parser<'a, K, Vec<O>>;

    /// Parse this parser separated by a delimiter (but keep only the content values).
    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A>;

    /// Succeeds only if this parser fails, and vice versa. Produces no value.
    fn not(self) -> impl Parser<'a, K, ()>;

    /// Surround this parser with padding (like whitespace) and return the result.
    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A> + Clone;

    /// Ignores the actual parsed value and replaces it with a given one.
    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a;

    /// Debug tracing combinator to help inspect parsing behavior.
    fn debug(self, label: &'static str) -> impl Parser<'a, K, O>;

    /// Apply another parser after this one, but return only the result of the first as well as the location after parsing.
    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, O>
    where
        P2: Parser<'a, K, A>;
}
