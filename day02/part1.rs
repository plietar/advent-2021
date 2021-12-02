mod common;
use common::*;

fn main() {
    let mut position = 0;
    let mut depth = 0;
    for c in parse() {
        match c {
            Command::Forward(n) => position += n,
            Command::Up(n) => depth -= n,
            Command::Down(n) => depth += n,
        }
    }
    println!("{}", position * depth)
}

