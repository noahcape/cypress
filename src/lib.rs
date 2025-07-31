/// The `parser` module contains the full implementation of the parser combinator library.
///
/// It exposes all core parser traits, combinators, and utilities for constructing
/// complex parsers from simple building blocks.
pub mod parser;

/// Common trait and implementations for parsing textlike values
pub mod text;

/// The `prelude` module re-exports commonly used items from the parser library.
///
/// This module is intended for convenient import, allowing users to bring
/// essential parser combinators and utilities into scope with a single `use`.
pub mod prelude {
    pub use super::choice;
    pub use super::select;
    pub use super::sequence;

    pub use super::text;

    pub use super::parser::{
        and::pand, any, between::pbetween, bind::pbind, core::*, debug::debug, delim::pdelim,
        ident::pident, into::pinto, just, many::pmany, not::pnot, or::por, padded::ppadded,
        pletter, pnum, pws, recursive::recursive, sat::psat, seq::pseq, utils::IntoPInput,
    };
}
