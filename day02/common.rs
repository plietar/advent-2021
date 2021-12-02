use std::io::stdin;
use std::str::FromStr;
use std::error::Error;
use std::io::BufRead;

pub enum Command {
    Forward(u64),
    Down(u64),
    Up(u64),
}

impl FromStr for Command {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split(' ').collect::<Vec<_>>();
        let n : u64 = parts[1].parse()?;
        match parts[0] {
            "forward" => Ok(Command::Forward(n)),
            "down" => Ok(Command::Down(n)),
            "up" => Ok(Command::Up(n)),
            _ => panic!(),
        }
    }
}

pub fn parse() -> Vec<Command> {
    let input = stdin();
    let input = input.lock();
    input.lines().map(|line| line.unwrap().parse::<Command>().unwrap()).collect()
}
