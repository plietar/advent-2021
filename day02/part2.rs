mod common;
use common::*;

fn main() {
    let mut position = 0;
    let mut depth = 0;
    let mut aim = 0;
    for c in parse() {
        match c {
            Command::Forward(n) => {
                position += n;
                depth += n * aim;
            },
            Command::Up(n) => aim -= n,
            Command::Down(n) => aim += n,
        }
    }
    println!("{}", position * depth)
}
