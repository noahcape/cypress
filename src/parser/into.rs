use crate::parser::*;

pub struct PInto<P, In, Out> {
    p: P,
    out: Out,
    _marker: PhantomData<In>,
}

pub fn pinto<P, In, Out>(p: P, out: Out) -> PInto<P, In, Out> {
    PInto {
        p,
        out,
        _marker: PhantomData,
    }
}

impl<'a, K, P, In, Out> ParserCore<'a, K, Out> for PInto<P, In, Out>
where
    K: PartialEq + Copy + Clone + 'a,
    P: Parser<'a, K, In>,
    Out: PartialEq + Clone,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, Out>, PFail<'a, K>> {
        let PSuccess { val: _, rest } = self.p.parse(i)?;

        Ok(PSuccess {
            val: self.out.clone(),
            rest,
        })
    }
}

impl<'a, K, P, In, Out> Parser<'a, K, Out> for PInto<P, In, Out>
where
    K: PartialEq + Copy + Clone + 'a,
    Out: PartialEq + Clone + 'a,
    P: Parser<'a, K, In>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (Out, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, Out>
    where
        T: Parser<'a, K, Out>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(Out) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, Out>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, Vec<Out>> {
        pmany(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<Out>>
    where
        PD: Parser<'a, K, A>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, ()> {
        pnot(self)
    }

    fn padded_by<Pr, A>(self, pad: Pr) -> impl Parser<'a, K, Out>
    where
        Pr: Parser<'a, K, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out_>(self, out: Out_) -> impl Parser<'a, K, Out_>
    where
        Out_: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }

    fn debug(self, label: &'static str) -> impl Parser<'a, K, Out> {
        debug(self, label)
    }
}
