/// For validating properties of input tokens
/// Idea taken from zester/chumsky
pub trait Char {
    fn is_digit(&self, radix: u32) -> bool;
    fn is_inline_whitespace(&self) -> bool;
    fn is_newline(&self) -> bool;
    fn is_whitespace(&self) -> bool;
    fn to_ascii(&self) -> Option<u8>;
}

impl Char for u8 {
    fn is_digit(&self, radix: u32) -> bool {
        (*self as char).is_digit(radix)
    }

    fn is_whitespace(&self) -> bool {
        (*self as char).is_ascii_whitespace()
    }

    fn to_ascii(&self) -> Option<u8> {
        self.is_ascii().then_some(*self)
    }

    fn is_inline_whitespace(&self) -> bool {
        self == &b' ' || self == &b'\t'
    }

    fn is_newline(&self) -> bool {
        [
            b'\n',   // Newline
            b'\r',   // Carriage return
            b'\x0B', // Vertical tab
            b'\x0C', // Form feed
        ]
        .as_slice()
        .contains(self)
    }
}

impl Char for char {
    fn is_digit(&self, radix: u32) -> bool {
        char::is_digit(*self, radix)
    }

    fn is_whitespace(&self) -> bool {
        char::is_whitespace(*self)
    }

    fn to_ascii(&self) -> Option<u8> {
        self.is_ascii().then_some(*self as u8)
    }

    fn is_inline_whitespace(&self) -> bool {
        self == &' ' || self == &'\t'
    }

    fn is_newline(&self) -> bool {
        [
            '\n',       // Newline
            '\r',       // Carriage return
            '\x0B',     // Vertical tab
            '\x0C',     // Form feed
            '\u{0085}', // Next line
            '\u{2028}', // Line separator
            '\u{2029}', // Paragraph separator
        ]
        .as_slice()
        .contains(self)
    }
}
