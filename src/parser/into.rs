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

impl<'a, K, I, P, In, Out> ParserCore<'a, K, I, Out> for PInto<P, In, Out>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P: Parser<'a, K, I, In>,
    Out: PartialEq + Clone,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, Out>, PFail<K, I>> {
        match self.p.parse(i) {
            Ok(PSuccess { val: _, rest }) => Ok(PSuccess {
                val: self.out.clone(),
                rest,
            }),
            Err(pfail) => Err(pfail),
        }
    }
}

impl<'a, K, I, P, In, Out> Parser<'a, K, I, Out> for PInto<P, In, Out>
where
    K: PartialEq + Copy + Clone + 'a,
    I: Iterator<Item = K> + Clone + 'a,
    Out: PartialEq + Clone + 'a,
    P: Parser<'a, K, I, In>,
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (Out, O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, Out>
    where
        T: Parser<'a, K, I, Out>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(Out) -> O2 + 'static,
        O2: 'a,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, Out>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<Out>> {
        pmany(self)
    }

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<Out>>
    where
        PD: Parser<'a, K, I, A>,
    {
        pdelim(self, delim)
    }

    fn not(self) -> impl Parser<'a, K, I, ()> {
        pnot(self)
    }

    fn padded_by<Pr, A>(self, pad: Pr) -> impl Parser<'a, K, I, Out>
    where
        Pr: Parser<'a, K, I, A> + Clone,
    {
        ppadded(self, pad)
    }

    fn into_<Out_>(self, out: Out_) -> impl Parser<'a, K, I, Out_>
    where
        Out_: PartialEq + Clone + 'a,
    {
        pinto(self, out)
    }
}
