use crate::parser::core::PInput;

pub trait IntoToken<K> {
    fn into_token(self) -> K;
}

impl<K> IntoToken<K> for K
where
    K: Copy,
{
    fn into_token(self) -> K {
        self
    }
}

impl IntoToken<u8> for char {
    fn into_token(self) -> u8 {
        self as u8
    }
}

pub trait IntoPInput<'a, T>
where
    T: PartialEq + Clone + Copy + 'a,
{
    fn into_input(self) -> PInput<'a, T>;
}

impl<'a, T> IntoPInput<'a, T> for &'a [T]
where
    T: PartialEq + Clone + Copy + 'a,
{
    fn into_input(self) -> PInput<'a, T> {
        PInput {
            tokens: self,
            loc: 0,
        }
    }
}

impl<'a> IntoPInput<'a, u8> for &'a str {
    fn into_input(self) -> PInput<'a, u8> {
        PInput {
            tokens: self.as_bytes(),
            loc: 0,
        }
    }
}
