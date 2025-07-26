use crate::parser::*;

pub struct PPaddedBy<P, PD, A> {
    p: P,
    pad: PD,
    _marker: PhantomData<A>,
}

pub fn ppadded<P, PD, A>(p: P, pad: PD) -> PPaddedBy<P, PD, A> {
    PPaddedBy {
        p,
        pad,
        _marker: PhantomData,
    }
}

impl<'a, K, O, P, PD, A> ParserCore<'a, K, O> for PPaddedBy<P, PD, A>
where
    K: PartialEq + Copy + Clone + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
    PD: Parser<'a, K, A> + Clone,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>> {
        let pad = self.pad.clone().many();

        let PSuccess {
            rest: after_pad1, ..
        } = pad.parse(i)?;

        let PSuccess {
            val,
            rest: after_main,
        } = self.p.parse(after_pad1)?;

        let PSuccess {
            rest: after_pad2, ..
        } = pad.parse(after_main)?;

        Ok(PSuccess {
            val,
            rest: after_pad2,
        })
    }
}

impl<'a, K, O, P, PD, B> Parser<'a, K, O> for PPaddedBy<P, PD, B>
where
    K: PartialEq + Copy + 'a,
    O: 'a,
    P: Parser<'a, K, O>,
    PD: Parser<'a, K, B> + Clone,
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

    fn delimited_by<PD_, A>(self, delim: PD_) -> impl Parser<'a, K, Vec<O>>
    where
        PD_: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn padded_by<Pad, A>(self, pad: Pad) -> impl Parser<'a, K, O>
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
