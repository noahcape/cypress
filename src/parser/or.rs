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

impl<'a, K, I, O, P1, P2> ParserCore<'a, K, I, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P1: Parser<'a, K, I, O>,
    P2: Parser<'a, K, I, O>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail<K, I>> {
        match self.p1.parse(i.clone()) {
            Ok(psuccess) => Ok(psuccess),
            Err(_) => match self.p2.parse(i) {
                Ok(psuccess) => Ok(psuccess),
                Err(pfail) => Err(pfail),
            },
        }
    }
}

impl<'a, K, I, O, P1, P2> Parser<'a, K, I, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O: 'a,
    P1: Parser<'a, K, I, O>,
    P2: Parser<'a, K, I, O>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (O, O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, O>
    where
        T: Parser<'a, K, I, O>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, L, R>(self, l: L, r: R) -> impl Parser<'a, K, I, O>
    where
        L: Parser<'a, K, I, A>,
        R: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<O>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<O>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, I, O>
    where
        P: Parser<'a, K, I, A> + Clone,
    {
        ppadded(self, pad)
    }
}
