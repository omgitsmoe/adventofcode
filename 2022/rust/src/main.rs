use std::env;
use std::io;

// declare subfolder as module here (could also use separate days.rs or days/mod.rs)
mod days {
    pub mod d01;
    pub mod d02;
}

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("Usage: aoc2022 <day:u8>")
    }
    let day: u8 = args[1].parse().expect("Could not convert day argument to an u8");

    match day {
        1 => days::d01::solve(),
        2 => days::d02::solve(),
        _ => unimplemented!(),
    };

    Ok(())
}
