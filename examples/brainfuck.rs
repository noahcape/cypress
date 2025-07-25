use parsec::{
    choice,
    parser::{
        core::{PSuccess, Parser, ParserCore, prepare},
        recursive::recursive,
        *,
    },
};

// See: https://gist.github.com/roachhd/dce54bec8ba55fb17d3a for an overview of BrainFuck
// > = increases memory pointer, or moves the pointer to the right 1 block.
// < = decreases memory pointer, or moves the pointer to the left 1 block.
// + = increases value stored at the block pointed to by the memory pointer
// - = decreases value stored at the block pointed to by the memory pointer
// [ = like c while(cur_block_value != 0) loop.
// ] = if block currently pointed to's value is not zero, jump back to [
// , = like c getchar(). input 1 character.
// . = like c putchar(). print 1 character to the console

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

fn main() {
    let parser = recursive(|expr| {
        Box::new(
            choice!(
                just('<').into_(Instruction::Left),
                just('>').into_(Instruction::Right),
                just('+').into_(Instruction::Increment),
                just('-').into_(Instruction::Decrement),
                just(',').into_(Instruction::Read),
                just('.').into_(Instruction::Write),
                expr.between(just('['), just(']'))
                    .map(|expr| Instruction::Loop(expr)),
            )
            .many(),
        )
    });

    let input = prepare("+++++[>>+<<-]".as_bytes());

    let expected_bf = vec![
        Instruction::Increment,
        Instruction::Increment,
        Instruction::Increment,
        Instruction::Increment,
        Instruction::Increment,
        Instruction::Loop(vec![
            Instruction::Right,
            Instruction::Right,
            Instruction::Increment,
            Instruction::Left,
            Instruction::Left,
            Instruction::Decrement,
        ]),
    ];

    match parser.parse(input) {
        Ok(PSuccess {
            val: actual_bf,
            rest: _,
        }) => assert_eq!(actual_bf, expected_bf),
        Err(_) => assert!(false),
    }
}
