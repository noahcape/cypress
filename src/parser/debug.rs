use crate::parser::*;

pub struct PDebug<P> {
    inner: P,
    label: &'static str,
}

pub fn debug<P>(inner: P, label: &'static str) -> PDebug<P> {
    PDebug { inner, label }
}

impl<'a, K, O, P> ParserCore<'a, K, O> for PDebug<P>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, O>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        match self.inner.parse(i) {
            Ok(psuccess) => {
                println!("Successfully parsed with {}", self.label);
                Ok(psuccess)
            }
            Err(PFail { error, span, rest }) => {
                println!(
                    "Failed {}: with msg: {} at position span ({}, {})",
                    self.label, error, span.0, span.1
                );
                Err(PFail { error, span, rest })
            }
        }
    }
}

impl<'a, K, O, Inner> Parser<'a, K, O> for PDebug<Inner>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    Inner: Parser<'a, K, O>,
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

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
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
}
