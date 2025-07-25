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
