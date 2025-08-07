//! # Cypress
//!
//! [![crates.io](https://img.shields.io/crates/v/cypress.svg)](https://crates.io/crates/cypress)
//! [![docs.rs](https://docs.rs/cypress/badge.svg)](https://docs.rs/cypress)
//!
//! Cypress is a parser combinator library for Rust, inspired by [FParsec] and [chumsky]. It aims to provide a
//! simple yet expressive framework for constructing parsers, especially for building small domain-specific
//! languages or interpreters.
//!
//! This library is still a work in progress, but it already supports recursive grammars, expressive combinators,
//! and error handling. Cypress emphasizes readability and composability, with an ergonomic macro-based DSL.
//!
//! ## Highlights
//!
//! - Recursive parsers via `recursive`
//! - Combinators for sequencing, mapping, branching, etc.
//! - Simple token-based parsing with custom input types
//! - Macros for more ergonomic parsing
//!
//! ## Example
//!
//! The following is a parser for [Brainfuck], demonstrating recursive parsing of a toy language:
//!
//! ```rust
//! use cypress::prelude::*;
//!
//! #[derive(Debug, Clone, PartialEq)]
//! enum Instruction {
//!     Left,
//!     Right,
//!     Increment,
//!     Decrement,
//!     Read,
//!     Write,
//!     Loop(Vec<Self>),
//! }
//!
//! fn bf_parser<'a>() -> impl Parser<'a, u8, Vec<Instruction>> {
//!     recursive(|expr| {
//!         let instr = choice!(
//!             select! {
//!                 '<' => Instruction::Left,
//!                 '>' => Instruction::Right,
//!                 '+' => Instruction::Increment,
//!                 '-' => Instruction::Decrement,
//!                 ',' => Instruction::Read,
//!                 '.' => Instruction::Write,
//!             },
//!             expr.many()
//!                 .between(just('['), just(']'))
//!                 .map(Instruction::Loop)
//!         );
//!
//!         Box::new(instr)
//!     })
//!     .many()
//!     .until_end()
//! }
//!
//! let input = b"+++++[>>+<<-]".into_input();
//! let result = bf_parser().parse(input);
//! ```
//!
//! More examples, including interpreters and simple language grammars, can be found in the `/examples` folder.
//!
//! ## Getting Started
//!
//! Add this to your `Cargo.toml`:
//!
//! ```toml,ignore
//! cypress = "0.2.1"
//! ```
//!
//! Then import the prelude:
//!
//! ```rust
//! use cypress::prelude::*;
//! ```
//!
//! ## Feedback
//!
//! This is my first published project — ideas, feedback, and PRs are very welcome!
//!
//! ## License
//!
//! Cypress is licensed under the BSD 3-Clause License. See `LICENSE` for details.
//!
//! [FParsec]: https://www.quanttec.com/fparsec/
//! [chumsky]: https://docs.rs/chumsky
//! [Brainfuck]: https://gist.github.com/roachhd/dce54bec8ba55fb17d3a

/// The `parser` module contains the full implementation of the parser combinator library.
///
/// It exposes all core parser traits, combinators, and utilities for constructing
/// complex parsers from simple building blocks.
pub mod parser;

/// Common trait and implementations for parsing textlike values
pub mod text;

/// Modules for displaying and organizing errors
pub mod error;

/// The `prelude` module re-exports commonly used items from the parser library.
///
/// This module is intended for convenient import, allowing users to bring
/// essential parser combinators and utilities into scope with a single `use`.
pub mod prelude {
    pub use super::choice;
    pub use super::precedence;
    pub use super::select;
    pub use super::sequence;
    pub use super::wrap;

    pub use super::error::*;
    pub use super::text::*;

    pub use super::parser::{
        and::pand, any, between::pbetween, bind::pbind, core::*, debug::debug, delim::pdelim,
        delim1::pdelim1, fold_left::pfoldl, ident::pident, ignore_then::pignore_then, into::pinto,
        just, many::pmany, many1::pmany1, map_error::pmap_error, map_with_span::pmap_with_span,
        not::pnot, or::por, padded::ppadded, pinlinews, pletter, pnum, pws, recursive::recursive,
        sat::psat, seq::pseq, then_ignore::pthen_ignore, until_end::puntil_end, utils::IntoPInput,
    };
}
