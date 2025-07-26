use crate::parser::*;
use std::{cell::RefCell, rc::Rc};

/// A type alias for a reference-counted, mutable, optionally initialized parser.
///
/// This allows creating recursive parsers by holding a reference to a parser
/// that can be set later (after its creation), enabling self-referential parsers.
type ParserRef<'a, K, O> = Rc<RefCell<Option<Box<dyn ParserCore<'a, K, O> + 'a>>>>;

/// A parser combinator that supports recursive parser definitions.
///
/// `PRecursive` allows you to define parsers that refer to themselves,
/// which is useful for parsing recursive grammars (e.g., expressions, nested structures).
///
/// It works by internally holding a mutable reference to a parser that is
/// initialized later, enabling self-references.
pub struct PRecursive<'a, K, O> {
    /// A shared, mutable optional boxed parser that can be initialized after creation.
    parser: ParserRef<'a, K, O>,
}

impl<'a, K, O> Clone for PRecursive<'a, K, O> {
    /// Clones the recursive parser by cloning the internal reference,
    /// allowing multiple handles to the same underlying parser.
    fn clone(&self) -> Self {
        Self {
            parser: Rc::clone(&self.parser),
        }
    }
}

/// Creates a new recursive parser by providing a function `f` that
/// receives a `PRecursive` instance and returns the actual parser implementation.
///
/// # Type Parameters
///
/// * `'a` - Lifetime of the input tokens.
/// * `K` - The token type.
/// * `O` - The output type of the parser.
/// * `F` - A function that takes a recursive parser and returns a boxed parser.
///
/// # Arguments
///
/// * `f` - A function that receives the recursive parser handle and returns
///   a boxed parser that can recursively call itself.
///
/// # Returns
///
/// A `PRecursive` instance implementing `ParserCore`, which supports
/// recursive parsing by delegating to the parser created inside `f`.
///
/// # Example
///
/// ```rust
/// use parsec::prelude::*;
/// let input = b"(1+(2+3))".into_input();
///
/// #[derive(Debug, PartialEq)]
/// enum AST {
///     Num(u32),
///     Expr(Box<AST>, Box<AST>),
/// }
///
/// let parser = recursive(|expr| {
///     Box::new(choice!(
///         pnum().map(|a: u8| AST::Num((a - b'0').into())),
///         just('(')
///             .then(expr.clone())
///             .then(just('+').then(expr))
///             .then(just(')'))
///             .map(|(((_, lhs), (_, rhs)), _)| AST::Expr(Box::new(lhs), Box::new(rhs)))
///     ))
/// });
///
/// match parser.parse(input) {
///     Ok(PSuccess { val, rest: _ }) => assert_eq!(
///         val,
///         AST::Expr(
///             Box::new(AST::Num(1)),
///             Box::new(AST::Expr(Box::new(AST::Num(2)), Box::new(AST::Num(3))))
///         )
///     ),
///     Err(_) => assert!(false),
/// }
/// ```
pub fn recursive<'a, K, O, F>(f: F) -> PRecursive<'a, K, O>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
    F: FnOnce(PRecursive<'a, K, O>) -> Box<dyn ParserCore<'a, K, O> + 'a>,
{
    let cell = Rc::new(RefCell::new(None));
    let rec = PRecursive {
        parser: cell.clone(),
    };

    // Create the actual parser using the recursive reference
    let real_parser = f(rec);

    // Store the real parser inside the shared cell
    *cell.borrow_mut() = Some(real_parser);

    PRecursive { parser: cell }
}

impl<'a, K, O> ParserCore<'a, K, O> for PRecursive<'a, K, O>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
{
    /// Parses input by delegating to the internally stored parser.
    ///
    /// Panics if the internal parser is not yet initialized (should never happen
    /// if created via `recursive` function).
    ///
    /// # Arguments
    ///
    /// * `i` - The parser input.
    ///
    /// # Returns
    ///
    /// The result of parsing using the internally stored parser.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        self.parser
            .borrow()
            .as_ref()
            .expect("Recursive parser not initialized")
            .parse(i)
    }
}

impl<'a, K, O> Parser<'a, K, O> for PRecursive<'a, K, O>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
    where
        T: Parser<'a, K, O>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, O> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, O>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
