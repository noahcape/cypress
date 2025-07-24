use crate::parser::*;

#[derive(Clone, Copy)]
pub struct PSeq<P1, P2> {
    p1: P1,
    p2: P2,
}

pub fn pseq<P1, P2>(p1: P1, p2: P2) -> PSeq<P1, P2> {
    PSeq { p1, p2 }
}

impl<'a, P1, P2, K, I, O1, O2> ParserCore<'a, K, I, (O1, O2)> for PSeq<P1, P2>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P1: Parser<'a, K, I, O1>,
    P2: Parser<'a, K, I, O2>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, (O1, O2)>, PFail<K, I>> {
        match self.p1.parse(i) {
            Ok(PSuccess { val: f_val, rest }) => match self.p2.parse(rest) {
                Ok(PSuccess { val: s_val, rest }) => Ok(PSuccess {
                    val: (f_val, s_val),
                    rest,
                }),
                Err(pfail) => Err(pfail),
            },
            Err(pfail) => Err(pfail),
        }
    }
}

impl<'a, P1, P2, K, I, O1, O2> Parser<'a, K, I, (O1, O2)> for PSeq<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    O1: 'a,
    O2: 'a,
    P1: Parser<'a, K, I, O1>,
    P2: Parser<'a, K, I, O2>,
{
    fn then<O3, T>(self, p2: T) -> impl Parser<'a, K, I, ((O1, O2), O3)>
    where
        T: Parser<'a, K, I, O3>,
        O3: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, (O1, O2)>
    where
        T: Parser<'a, K, I, (O1, O2)>,
    {
        por(self, p2)
    }

    fn map<O3, F>(self, f: F) -> impl Parser<'a, K, I, O3>
    where
        F: Fn((O1, O2)) -> O3 + 'static,
        O3: 'a,
    {
        pbind(self, f)
    }

    fn between<A, L, R>(self, l: L, r: R) -> impl Parser<'a, K, I, (O1, O2)>
    where
        L: Parser<'a, K, I, A>,
        R: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<(O1, O2)>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<(O1, O2)>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, I, (O1, O2)>
    where
        P: Parser<'a, K, I, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, I, Out>
    where
        Out: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }
}
