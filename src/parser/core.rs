#[derive(Clone, Debug)]
pub struct PInput<'a, T: PartialEq + Copy + Clone + 'a> {
    pub tokens: &'a [T],
    pub loc: usize,
}

pub fn prepare<'a, T>(i: &'a [T]) -> PInput<'a, T>
where
    T: PartialEq + Copy + Clone,
{
    PInput { tokens: i, loc: 0 }
}

#[derive(Clone)]
pub struct PSuccess<'a, T, O>
where
    T: PartialEq + Copy + Clone,
{
    pub val: O,
    pub rest: PInput<'a, T>,
}

pub struct PFail<'a, T>
where
    T: PartialEq + Copy + Clone,
{
    pub error: String,
    pub span: (usize, usize),
    pub rest: PInput<'a, T>,
}

pub trait ParserCore<'a, K, O>
where
    K: PartialEq + Copy + Clone + 'a,
{
    fn parse(&self, i: PInput<'a, K>) -> Result<PSuccess<'a, K, O>, PFail<'a, K>>;
}

pub trait Parser<'a, K: PartialEq + Copy + Clone + 'a, O>: ParserCore<'a, K, O> + Sized {
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
    where
        T: Parser<'a, K, O2>,
        O2: 'a;

    fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
    where
        T: Parser<'a, K, O>;

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a;

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
    where
        P1: Parser<'a, K, A>,
        P2: Parser<'a, K, A>;

    fn many(self) -> impl Parser<'a, K, Vec<O>>;

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, Vec<O>>
    where
        PD: Parser<'a, K, A>;

    fn not(self) -> impl Parser<'a, K, ()>;

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, O>
    where
        P: Parser<'a, K, A> + Clone;

    fn into_<Out>(self, out: Out) -> impl Parser<'a, K, Out>
    where
        Out: PartialEq + Clone + 'a;
}
