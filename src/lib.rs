/// The `parser` module contains the full implementation of the parser combinator library.
///
/// It exposes all core parser traits, combinators, and utilities for constructing
/// complex parsers from simple building blocks.
pub mod parser;

/// The `prelude` module re-exports commonly used items from the parser library.
///
/// This module is intended for convenient import, allowing users to bring
/// essential parser combinators and utilities into scope with a single `use`.
///
/// # Re-exports include:
/// - Choice combinators (`choice` macro)
/// - Core parser traits and input types
/// - Common parser combinators such as:
///   - `pand` (and combinator)
///   - `any` (match any token)
///   - `pbetween` (parse between two delimiters)
///   - `pbind` (monadic bind)
///   - `debug` (debugging combinator)
///   - `pdelim` (delimited sequences)
///   - `pident` (identifier parser)
///   - `pinto` (map parser output)
///   - `just` (exact token match)
///   - `pmany` (many repetitions)
///   - `pnot` (negation parser)
///   - `por` (or combinator)
///   - `ppadded` (padding combinator)
///   - `pletter`, `pnum`, `pws` (character class parsers)
///   - `recursive` (recursive parser combinator)
///   - `psat` (predicate satisfaction parser)
///   - `pseq` (sequence combinator)
///   - `IntoPInput` (input conversion trait)
pub mod prelude {
    pub use super::choice;

    pub use super::parser::{
        and::pand, any, between::pbetween, bind::pbind, core::*, debug::debug, delim::pdelim,
        ident::pident, into::pinto, just, many::pmany, not::pnot, or::por, padded::ppadded,
        pletter, pnum, pws, recursive::recursive, sat::psat, seq::pseq, utils::IntoPInput,
    };
}
