use crate::parser::*;

/// A parser combinator that applies the inner parser `p` zero or more times,
/// collecting all successful parse results into a vector.
///
/// This combinator repeatedly applies the parser `p` until it fails,
/// and returns a vector of all successfully parsed values.
///
/// # Type Parameters
///
/// * `P` - The parser type to apply repeatedly.
pub struct PMany<P> {
    /// The inner parser to apply repeatedly.
    p: P,
}

/// Creates a new `PMany` parser combinator that applies parser `p` zero or more times.
///
/// # Arguments
///
/// * `p` - The parser to apply repeatedly.
///
/// # Returns
///
/// A `PMany` instance implementing `ParserCore` that collects multiple parse results
/// into a `Vec`.
pub fn pmany<P>(p: P) -> PMany<P> {
    PMany { p }
}

impl<'a, K, O, P> ParserCore<'a, K, Vec<O>> for PMany<P>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
{
    /// Parses zero or more occurrences of the inner parser `p`.
    ///
    /// This method repeatedly attempts to parse the input using `p`, pushing
    /// each successful parse result into a vector. Parsing stops when `p` fails.
    ///
    /// # Arguments
    ///
    /// * `i` - The input to parse.
    ///
    /// # Returns
    ///
    /// Always returns `Ok(PSuccess)` with a vector of all parsed values (which
    /// may be empty) and the remaining input.
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Vec<O>>, PFail<'a, K>> {
        let mut vals: Vec<O> = vec![];
        let mut input = i;

        // Repeatedly parse input with `p` until it fails
        while let Ok(PSuccess { val, rest }) = self.p.parse(input.clone()) {
            vals.push(val);
            input = rest;
        }

        // Return all successfully parsed values and the remaining input
        Ok(PSuccess {
            val: vals,
            rest: input,
        })
    }
}

impl<'a, K, O, P> Parser<'a, K, Vec<O>> for PMany<P>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (Vec<O>, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, Vec<O>>
    where
        T: Parser<'a, K, Vec<O>>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(Vec<O>) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, Vec<O>>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<Vec<O>>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<Vec<O>>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, Vec<O>>
    where
        Pad: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, Vec<O>> {
        debug(self, label)
    }

    fn and<P2, A>(self, second: P2) -> impl Parser<'a, K, Vec<O>>
    where
        P2: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
