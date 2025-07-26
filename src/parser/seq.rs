use crate::parser::*;

#[derive(Clone, Copy)]
pub struct PSeq<P1, P2> {
    p1: P1,
    p2: P2,
}

pub fn pseq<P1, P2>(p1: P1, p2: P2) -> PSeq<P1, P2> {
    PSeq { p1, p2 }
}

impl<'a, P1, P2, K, O1, O2> ParserCore<'a, K, (O1, O2)> for PSeq<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    P1: Parser<'a, K, O1>,
    P2: Parser<'a, K, O2>,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, (O1, O2)>, PFail<'a, K>> {
        let PSuccess { val: f_val, rest } = self.p1.parse(i)?;
        let PSuccess { val: s_val, rest } = self.p2.parse(rest)?;

        Ok(PSuccess {
            val: (f_val, s_val),
            rest,
        })
    }
}

impl<'a, P1, P2, K, O1, O2> Parser<'a, K, (O1, O2)> for PSeq<P1, P2>
where
    K: PartialEq + Copy + Clone + 'a,
    O1: 'a,
    O2: 'a,
    P1: Parser<'a, K, O1>,
    P2: Parser<'a, K, O2>,
{
    fn then<O3, T>(self, p2: T) -> impl Parser<'a, K, ((O1, O2), O3)>
    where
        T: Parser<'a, K, O3>,
        O3: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, (O1, O2)>
    where
        T: Parser<'a, K, (O1, O2)>,
    {
        por(self, p2)
    }

    fn map<O3, F>(self, f: F) -> impl Parser<'a, K, O3>
    where
        F: Fn((O1, O2)) -> O3 + 'static,
        O3: 'a,
    {
        pbind(self, f)
    }

    fn between<A, L, R>(self, l: L, r: R) -> impl Parser<'a, K, (O1, O2)>
    where
        L: Parser<'a, K, A>,
        R: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<(O1, O2)>> {
        pmany(self)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<(O1, O2)>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, (O1, O2)>
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

    fn debug(self, label: &'static str) -> impl Parser<'a, K, (O1, O2)> {
        debug(self, label)
    }

    fn and<P2_, A>(self, second: P2_) -> impl Parser<'a, K, (O1, O2)>
    where
        P2_: Parser<'a, K, A>,
    {
        pand(self, second)
    }
}
