/// A convenient macro to create a choice parser from two or more parsers.
///
/// This macro recursively combines multiple parsers into nested `por` combinators,
/// allowing you to write `choice!(p1, p2, p3, ...)` instead of nested calls.
///
/// # Examples
///
/// ```
/// use parsec::prelude::*;
/// let input = "B".into_input();
/// let parser1 = just('A').or(just('B'));
/// let parser2 = choice!(just('A'), just('B'));
///
/// // Note that these two parsers are the same
/// match parser1.parse(input.clone()) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'B'),
///     Err(_) => assert!(false),
/// };
///
/// match parser2.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, b'B'),
///     Err(_) => assert!(false),
/// };
/// ```
///
/// This is equivalent to `por(p1, por(p2, p3))`.
#[macro_export]
macro_rules! choice {
    ($p:expr, $q:expr $(,)?) => {
        $crate::parser::or::por($p, $q)
    };
    ($p:expr, $( $rest:expr ),* $(,)?) => {
        $crate::parser::or::por($p, $crate::choice!($($rest),*))
    };
}

/// Macro for conveniently creating a match and map like statement.
///
/// This macro recursively combines multiple literal or parsers along with
/// an into_(..) clause to convert the first successfully parsed token into
/// the associated value.
///
/// Note that using a declerative macro one cannot differentiate between char, u8,
/// and str. Thus, one must use `(pident(..))` to match a str. Further note that
/// you must wrap such parsers in parens to properly parse with macros.
///
/// # Examples
///
/// ```
/// use parsec::prelude::*;
/// let input = "Luke".into_input();
///
/// #[derive(Clone, PartialEq, Debug)]
/// enum StarWarsCharacters {
///     Luke,
///     ObiWan,
///     DarthVader,
///     Yoda
/// };
///
/// let select_parser = select! {
///     (pident("ObiWan")) => StarWarsCharacters::ObiWan,
///     (pident("Yoda")) => StarWarsCharacters::Yoda,
///     (pident("DarthVade")) => StarWarsCharacters::DarthVader,
///     (pident("Luke")) => StarWarsCharacters::Luke,
/// };
///
/// // equivalent parser using choice!(..)
/// let choice_parser = choice!(
///     (pident("ObiWan")).into_(StarWarsCharacters::ObiWan),
///     (pident("Yoda")).into_(StarWarsCharacters::Yoda),
///     (pident("DarthVade")).into_(StarWarsCharacters::DarthVader),
///     (pident("Luke")).into_(StarWarsCharacters::Luke),
/// );
///
/// let Some(PSuccess {val: select_val, rest: _ }) = select_parser.parse(input.clone()).ok() else { panic!() };
/// let Some(PSuccess {val: choice_val, rest: _ }) = choice_parser.parse(input).ok() else { panic!() };
/// assert_eq!(select_val, choice_val);
///
/// // or use just chars to make parsing more ergonomic
/// #[derive(Clone, PartialEq, Debug)]
/// enum Letter {
///     A, B, C, D
/// };
///
/// let letter_parser = select! {
///     'A' => Letter::A, // expands to just('A') => Letter::A like above example
///     'B' => Letter::B,
///     'C' => Letter::C,
///     'D' => Letter::D,
/// }.many();
///
/// let input = "ADBC".into_input();
///
/// match letter_parser.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(val, vec![Letter::A, Letter::D, Letter::B, Letter::C]),
///     Err(_) => assert!(false)
/// }
///
/// ```
#[macro_export]
macro_rules! select {
    {
        $first:tt => $into:expr,
        $second:tt => $into2:expr $(,)?
    } => {
        $crate::parser::or::por(
            $crate::parser::bind::pbind($crate::wrap!($first), |_| $into),
            $crate::parser::bind::pbind($crate::wrap!($second), |_| $into2),
        )
    };

    {
        $first:tt => $into:expr,
        $( $rest_p:tt => $rest_into:expr ),* $(,)?
    } => {
        $crate::parser::or::por(
            $crate::parser::bind::pbind($crate::wrap!($first), |_| $into),
            $crate::select!($( $rest_p => $rest_into ),*)
        )
    }
}

/// Macro for ergonomic parsing in sequence.
///
/// This macro recursively combines multiple parser or literals
/// into a sequene parser using pseq. Optionally one can use `=>`
/// along with a closure to map the parser result into a useful result.
///
/// See `[crate::select!]` for details about passing parser versus literals.
///
/// # Examples
///
/// ```
/// use parsec::prelude::*;
///
/// let input = "1+2".into_input();
///
/// #[derive(PartialEq, Debug)]
/// enum Expr {
///     Num(u8),
///     Add(Box<Expr>, Box<Expr>)
/// };
///
/// let parser = sequence!(
///     (pnum()) > '+' > (pnum()) => |(a, (_, b))| Expr::Add(Box::new(Expr::Num(a)), Box::new(Expr::Num(b)))
/// );
///
/// match parser.parse(input) {
///     Ok(PSuccess {val, rest: _ }) => assert_eq!(val, Expr::Add(Box::new(Expr::Num(b'1')), Box::new(Expr::Num(b'2')))),
///     Err(_) => assert!(false),
/// };
/// ```
#[macro_export]
macro_rules! sequence {
    ($first:tt $(> $tail:tt)+ => $map:expr) => {
        $crate::parser::bind::pbind(
            $crate::sequence!(@chain $first $(> $tail)+),
            $map
        )
    };

    ($first:tt $(> $tail:tt)+) => {
        $crate::sequence!(@chain $first $(> $tail)+)
    };

    (@chain $head:tt > $($tail:tt)+) => {
        $crate::parser::seq::pseq(
            $crate::wrap!($head),
            $crate::sequence!(@chain $($tail)+)
        )
    };

    (@chain $last:tt) => {
        $crate::wrap!($last)
    };

}

#[macro_export]
macro_rules! wrap {
    (($e:expr)) => {
        $e
    };

    ($parser:ident) => {
        $parser
    };

    ($ch:tt) => {{ $crate::parser::just($ch) }};
}
