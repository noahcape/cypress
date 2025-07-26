pub mod parser;

pub mod prelude {
    pub use super::choice;

    pub use super::parser::{
        and::pand, any, between::pbetween, bind::pbind, core::*, debug::debug, delim::pdelim,
        ident::pident, into::pinto, just, many::pmany, not::pnot, or::por, padded::ppadded,
        pletter, pnum, pws, recursive::recursive, sat::psat, seq::pseq, utils::IntoPInput,
    };
}
