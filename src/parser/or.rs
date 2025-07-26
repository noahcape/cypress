use crate::parser::*;

pub struct POr<P1, P2> {
    p1: P1,
    p2: P2,
}

pub fn por<P1, P2>(p1: P1, p2: P2) -> POr<P1, P2> {
    POr { p1, p2 }
}

#[macro_export]
macro_rules! choice {
    ($p:expr, $q:expr $(,)?) => {$crate::parser::or::por($p, $q)};
    ($p:expr, $( $rest:expr ),* $(,)?) => {$crate::parser::or::por($p, $crate::choice!($($rest),*))};
}

impl<'a, K, O, P1, P2> ParserCore<'a, K, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, O>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        match self.p1.parse(i.clone()) {
            Ok(psuccess) => Ok(psuccess),
            Err(_) => self.p2.parse(i),
        }
    }
}

impl<'a, K, O, P1, P2> Parser<'a, K, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, O>,
    P2: Parser<'a, K, O>,
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

    fn between<A, L, R>(self, l: L, r: R) -> impl Parser<'a, K, O>
    where
        L: Parser<'a, K, A>,
        R: Parser<'a, K, A>,
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

    fn and<P2_, A>(self, second: P2_) -> impl Parser<'a, K, O>
    where
        P2_: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
