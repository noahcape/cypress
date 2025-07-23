use std::{fmt::Display, marker::PhantomData, sync::Arc};

#[derive(Clone, Copy)]
pub struct PInput<T: PartialEq + Copy, I: Iterator<Item = T>> {
    tokens: I,
    loc: u32,
}

pub fn prepare<T: PartialEq + Copy, I: Iterator<Item = T>>(i: I) -> PInput<T, I> {
    PInput { tokens: i, loc: 0 }
}

#[derive(Clone, Copy)]
pub struct PSuccess<T: PartialEq + Copy, I: Iterator<Item = T>, O> {
    pub val: O,
    pub rest: PInput<T, I>,
}

pub struct PFail {
    pub error: String,
    pub span: (u32, u32),
}

pub trait Parser<'a, K: PartialEq + Copy, I: Iterator<Item = K>, O> {
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail>;

    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (O, O2)>
    where
        T: Parser<'a, K, I, O2>;

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, O>
    where
        T: Parser<'a, K, I, O>;

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(O) -> O2 + 'static;

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>;

    fn many(self) -> impl Parser<'a, K, I, Vec<O>>;

    // TODO
    // fn delimited_by(self, tok: K) -> impl Parser<'a, K, I, Vec<O>;

    // fn not(self) -> impl Parser<'a, K, I, O>;

    // fn padded(self) -> impl Parser<'a, K, I, O>;
}

pub struct PMany<P> {
    p: P,
}

fn pmany<P>(p: P) -> PMany<P> {
    PMany { p }
}

impl<'a, K, I, O, P> Parser<'a, K, I, Vec<O>> for PMany<P>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P: Parser<'a, K, I, O>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, Vec<O>>, PFail> {
        let mut vals: Vec<O> = vec![];
        let mut input = i;

        loop {
            match self.p.parse(input.clone()) {
                Ok(PSuccess { val, rest }) => {
                    vals.push(val);
                    input = rest;
                }
                Err(_) => break,
            }
        }

        Ok(PSuccess {
            val: vals,
            rest: input,
        })
    }

    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (Vec<O>, O2)>
    where
        T: Parser<'a, K, I, O2>,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, Vec<O>>
    where
        T: Parser<'a, K, I, Vec<O>>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(Vec<O>) -> O2 + 'static,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, Vec<O>>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<Vec<O>>> {
        pmany(self)
    }
}

pub struct PBind<P, O1, O2> {
    p: P,
    f: Arc<dyn Fn(O1) -> O2>,
}

fn pbind<P, F, O1, O2>(p: P, f: F) -> PBind<P, O1, O2>
where
    F: Fn(O1) -> O2 + 'static,
{
    PBind { p, f: Arc::new(f) }
}

impl<'a, P, K, I, O1, O2> Parser<'a, K, I, O2> for PBind<P, O1, O2>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P: Parser<'a, K, I, O1>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O2>, PFail> {
        match self.p.parse(i) {
            Ok(PSuccess { val, rest }) => Ok(PSuccess {
                val: (self.f)(val),
                rest,
            }),
            Err(pfail) => Err(pfail),
        }
    }

    fn then<O3, T: Parser<'a, K, I, O3>>(self, p2: T) -> impl Parser<'a, K, I, (O2, O3)> {
        pseq(self, p2)
    }

    fn or<T: Parser<'a, K, I, O2>>(self, p2: T) -> impl Parser<'a, K, I, O2> {
        por(self, p2)
    }

    fn map<O3, F>(self, f: F) -> impl Parser<'a, K, I, O3>
    where
        F: Fn(O2) -> O3 + 'static,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O2>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<O2>> {
        pmany(self)
    }
}

#[derive(Clone)]
pub struct PSat<K: PartialEq> {
    test: Arc<dyn Fn(K) -> bool>,
    condition: String,
}

impl<'a, K, I> Parser<'a, K, I, K> for PSat<K>
where
    K: PartialEq + Copy,
    I: Iterator<Item = K> + Clone,
{
    fn parse(&self, mut i: PInput<K, I>) -> Result<PSuccess<K, I, K>, PFail> {
        match i.tokens.next() {
            Some(tok) => {
                if (self.test)(tok) {
                    return Ok(PSuccess {
                        val: tok,
                        rest: PInput {
                            tokens: i.tokens.clone(),
                            loc: i.loc + 1,
                        },
                    });
                } else {
                    Err(PFail {
                        error: self.condition.clone(),
                        span: (i.loc, i.loc + 1),
                    })
                }
            }
            None => Err(PFail {
                error: format!("No token to read"),
                span: (i.loc, i.loc),
            }),
        }
    }

    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (K, O2)>
    where
        T: Parser<'a, K, I, O2>,
    {
        pseq(self, p2)
    }

    fn or<T>(self, p2: T) -> impl Parser<'a, K, I, K>
    where
        T: Parser<'a, K, I, K>,
    {
        por(self, p2)
    }

    fn map<O2, F>(self, f: F) -> impl Parser<'a, K, I, O2>
    where
        F: Fn(K) -> O2 + 'static,
    {
        pbind(self, f)
    }

    fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, K>
    where
        P1: Parser<'a, K, I, A>,
        P2: Parser<'a, K, I, A>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<K>> {
        pmany(self)
    }
}

pub fn psat<K, F>(test: F, condition: String) -> PSat<K>
where
    K: PartialEq + Display,
    F: Fn(K) -> bool + 'static,
{
    let func = move |input: K| test(input);

    PSat {
        test: Arc::new(func),
        condition,
    }
}

#[derive(Clone, Copy)]
pub struct PSeq<P1, P2> {
    p1: P1,
    p2: P2,
}

impl<'a, P1, P2, K, I, O1, O2> Parser<'a, K, I, (O1, O2)> for PSeq<P1, P2>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P1: Parser<'a, K, I, O1>,
    P2: Parser<'a, K, I, O2>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, (O1, O2)>, PFail> {
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

    fn then<O3, T>(self, p2: T) -> impl Parser<'a, K, I, ((O1, O2), O3)>
    where
        T: Parser<'a, K, I, O3>,
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
}

pub fn pseq<P1, P2>(p1: P1, p2: P2) -> PSeq<P1, P2> {
    PSeq { p1, p2 }
}

pub struct POr<P1, P2> {
    p1: P1,
    p2: P2,
}

impl<'a, K, I, O, P1, P2> Parser<'a, K, I, O> for POr<P1, P2>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    P1: Parser<'a, K, I, O>,
    P2: Parser<'a, K, I, O>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail> {
        match self.p1.parse(i.clone()) {
            Ok(psuccess) => Ok(psuccess),
            Err(_) => match self.p2.parse(i) {
                Ok(psuccess) => Ok(psuccess),
                Err(pfail) => Err(pfail),
            },
        }
    }

    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (O, O2)>
    where
        T: Parser<'a, K, I, O2>,
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
}

pub fn por<P1, P2>(p1: P1, p2: P2) -> POr<P1, P2> {
    POr { p1, p2 }
}

#[macro_export]
macro_rules! choice {
    ($p:expr, $q:expr $(,)?) => {por($p, $q)};
    ($p:expr, $( $rest:expr ),* $(,)?) => {por($p, $crate::choice!($($rest),*))};
}

pub struct PBetween<L, P, R, A> {
    l: L,
    p: P,
    r: R,
    _marker: PhantomData<A>,
}

impl<'a, K, I, O, L, P, R, A> Parser<'a, K, I, O> for PBetween<L, P, R, A>
where
    K: PartialEq + Copy + Clone,
    I: Iterator<Item = K> + Clone,
    L: Parser<'a, K, I, A>,
    P: Parser<'a, K, I, O>,
    R: Parser<'a, K, I, A>,
{
    fn parse(&self, i: PInput<K, I>) -> Result<PSuccess<K, I, O>, PFail> {
        match self.l.parse(i) {
            Ok(PSuccess { val: _, rest }) => match self.p.parse(rest) {
                Ok(PSuccess { val, rest }) => match self.r.parse(rest) {
                    Ok(PSuccess { val: _, rest }) => Ok(PSuccess { val, rest }),
                    Err(pfail) => Err(pfail),
                },
                Err(pfail) => Err(pfail),
            },
            Err(pfail) => Err(pfail),
        }
    }

    fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, I, (O, O2)>
    where
        T: Parser<'a, K, I, O2>,
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
    {
        pbind(self, f)
    }

    fn between<B, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, I, O>
    where
        P1: Parser<'a, K, I, B>,
        P2: Parser<'a, K, I, B>,
    {
        pbetween(l, self, r)
    }

    fn many(self) -> impl Parser<'a, K, I, Vec<O>> {
        pmany(self)
    }
}

pub fn pbetween<L, P, R, A>(l: L, p: P, r: R) -> PBetween<L, P, R, A> {
    PBetween {
        l,
        p,
        r,
        _marker: PhantomData,
    }
}

pub fn pitem<K>(c: K) -> PSat<K>
where
    K: PartialEq + Display + Copy + 'static,
{
    psat(
        Box::new(move |i: K| i.eq(&c)),
        format!("Token doesn't match {}", c),
    )
}

pub fn pnum<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(move |i: K| Into::<char>::into(i).is_ascii_digit()),
        format!("Token is not a number"),
    )
}

pub fn pletter<K>() -> PSat<K>
where
    K: PartialEq + Display + Copy + Into<char> + 'static,
{
    psat(
        Box::new(|i: K| Into::<char>::into(i).is_ascii_alphabetic()),
        format!("Token is not a letter."),
    )
}

#[test]
fn t_pitem() {
    let input = prepare("ABC".chars());

    let parser = pitem('A');

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_then() {
    let input = prepare("ABC".chars());

    let parser = pitem('A').then(pitem('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, ('A', 'B')),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_or() {
    let input = prepare("B".chars());

    let parser = pitem('A').or(pitem('B'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'B'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_map() {
    let input = prepare("A".chars());

    let parser = pitem('A').map(|_| 1);

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 1),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_choice() {
    let input = prepare("A".chars());

    let parser = choice!(pitem('C'), pitem('A'), pitem('C'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pbetween() {
    let input = prepare("[A]".chars());

    let parser = pbetween(pitem('['), pitem('A'), pitem(']'));

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, 'A'),
        Err(_) => assert!(false),
    }
}

#[test]
fn t_pmany() {
    let input = prepare("ABCDE".chars());

    let parser = pletter().many();

    match parser.parse(input) {
        Ok(PSuccess { val, rest: _ }) => assert_eq!(val, ['A', 'B', 'C', 'D', 'E']),
        Err(_) => assert!(false),
    }
}
