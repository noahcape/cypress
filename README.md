[![crates.io](https://img.shields.io/crates/v/cypress.svg)](https://crates.io/crates/cypress)
[![crates.io](https://docs.rs/cypress/badge.svg)](https://docs.rs/cypress)

# Cypress

A parser library inspired by a scaled down derivative of the FParsec combinator library in F#.

I was also inspired by chumsky a very elegant parser combinator library which I have enjoyed using!

The goal of this project was to develop a simple parser combinator library with which people can write and parse expressive languages with ease. It is still a work in progress and there are areas that still need to be cleaned up but I believe that I have achieved my goal.

## Example

I have populated `/examples` with a parser of certain simple languages, along with an evaluator for a select few, to show using this library by example. I hope to populate one or two more sophisticated implementations soon. Below is a simple parser for [BrainFuck](https://gist.github.com/roachhd/dce54bec8ba55fb17d3a) which seems to be the go to simple language parser that libraries refer to, so here it is.

```rust,ignore
use cypress::prelude::*;

#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    Left,
    Right,
    Increment,
    Decrement,
    Read,
    Write,
    Loop(Vec<Self>),
}

fn bf_parser<'a>() -> impl Parser<'a, u8, Vec<Instruction>> {
    recursive(|expr| {
        Box::new(
            choice!(
                select! {
                    '<' => Instruction::Left,
                    '>' => Instruction::Right,
                    '+' => Instruction::Increment,
                    '-' => Instruction::Decrement,
                    ',' => Instruction::Read,
                    '.' => Instruction::Write,
                },
                expr.between(just('['), just(']'))
                    .map(|expr| Instruction::Loop(expr))
            )
            .many(),
        )
    })
    // Current way to go until the end of file
    .then(any().not())
    .map(|(bf, _)| bf);
}

let input = b"+++++[>>+<<-]".into_input();

// parse bf input
bf_parser.parse(input);
```

## Note

This is my first published project so if you have thoughts please feel free to open an issue a PR with ideas and fixes!

## License

cypress is licensed under the BSD 3 license (see `LICENSE` in the main repository).
