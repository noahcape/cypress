#[derive(Clone, Copy, Debug)]
pub struct PInput<T: PartialEq + Copy, I: Iterator<Item = T>> {
    pub tokens: I,
    pub loc: u32,
}

pub fn prepare<T: PartialEq + Copy, I: Iterator<Item = T>>(i: I) -> PInput<T, I> {
    PInput { tokens: i, loc: 0 }
}

#[derive(Clone, Copy)]
pub struct PSuccess<T: PartialEq + Copy, I: Iterator<Item = T>, O> {
    pub val: O,
    pub rest: PInput<T, I>,
}

pub struct PFail<K: PartialEq + Copy, I: Iterator<Item = K>> {
    pub error: String,
    pub span: (u32, u32),
    pub rest: PInput<K, I>,
}

pub trait ParserCore<'a, K, I, O>
where
    K: PartialEq + Copy,
    I: Iterator<Item = K>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail<K, I>>;
}

pub trait Parser<'a, K: PartialEq + Copy, I: Iterator<Item = K>, O>:
    ParserCore<'a, K, I, O> + Sized
{
    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (O, O2)>
    where
        T: Parser<'a, K, I, O2>,
        O2: 'a;

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, O>
    where
        T: Parser<'a, K, I, O>;

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(O) -> O2 + 'static,
        O2: 'a;

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>;

    fn many(self) -> impl Parser<'a, K, I, Vec<O>>;

    fn delimited_by<PD, A>(self, delim: PD) -> impl Parser<'a, K, I, Vec<O>>
    where
        PD: Parser<'a, K, I, A>;

    fn not(self) -> impl Parser<'a, K, I, ()>;

    fn padded_by<P, A>(self, pad: P) -> impl Parser<'a, K, I, O>
    where
        P: Parser<'a, K, I, A> + Clone;
}