use crate::parser::core::PInput;

/// A trait for converting a value into a parser token of type `K`.
///
/// This trait allows for flexible conversion into token types used by parsers.
/// For example, it enables converting a `char` into a `u8` token.
pub trait IntoToken<K> {
    /// Converts `self` into a token of type `K`.
    fn into_token(self) -> K;
}

/// Default implementation of `IntoToken` for types that are already tokens.
///
/// Simply returns the value itself.
impl<K> IntoToken<K> for K {
    fn into_token(self) -> K {
        self
    }
}

/// Implementation of `IntoToken<u8>` for `char`.
///
/// Converts a `char` into a `u8` by casting (truncates Unicode codepoints > 255).
impl IntoToken<u8> for char {
    fn into_token(self) -> u8 {
        self as u8
    }
}

/// A trait for converting various input types into the parser input type `PInput`.
///
/// This allows convenient creation of parser input from slices or strings.
pub trait IntoPInput<'a, T>
where
    T: PartialEq + Clone + 'a,
{
    /// Converts `self` into a `PInput` that the parser can consume.
    fn into_input(self) -> PInput<'a, T>;
}

/// Converts a slice of tokens `&[T]` into `PInput`.
///
/// The input starts at location 0 with the full slice available as tokens.
impl<'a, T> IntoPInput<'a, T> for &'a [T]
where
    T: PartialEq + Clone + 'a,
{
    fn into_input(self) -> PInput<'a, T> {
        PInput {
            tokens: self,
            loc: 0,
        }
    }
}

/// Converts a string slice `&str` into `PInput<u8>` by interpreting
/// the string as a byte slice (`&[u8]`).
///
/// This is useful for parsers that consume bytes instead of characters.
impl<'a> IntoPInput<'a, u8> for &'a str {
    fn into_input(self) -> PInput<'a, u8> {
        PInput {
            tokens: self.as_bytes(),
            loc: 0,
        }
    }
}
