use std::vec;
use std::fs;
use std::io;

fn get_option(byte: u8) -> i32 {
    match byte {
        // b'C' to get a u8 instead of char (since char iirc is a code point - not just u8)
        b'A' | b'X' => 1,
        b'B' | b'Y' => 2,
        b'C' | b'Z' => 3,
        _ => panic!("Unexpected option!"),
    }
}

#[derive(Copy, Clone, Debug)]
enum Outcome {
    DRAW = 3,
    LOSE = 0,
    WIN = 6,
}
const OUTCOMES: [Outcome; 3] = [Outcome::DRAW, Outcome::LOSE, Outcome::WIN];

pub fn solve() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d02.in")?;

    let mut points = 0;
    let mut points2 = 0;
    for line in contents.split('\n') {
        if line == "" { continue };
        println!("{}", line);

        let line_bytes = line.as_bytes();
        let opponent = get_option(line_bytes[0]);
        let me = get_option(line_bytes[2]);
        // rem_euclid won't have negative outcomes when doing 'modulo'
        let outcome_idx = (opponent - me).rem_euclid(3);
        let outcome = OUTCOMES[outcome_idx as usize];
        points += outcome as i32;
        points += me;

        // part2
        // if opponent chooses rock:
        // need to choose same to DRAW; +1 (so paper) to WIN; +2 (so scissors) to LOSE;
        // if opponent chooses paper:
        // need to choose same to DRAW; +1 (so scissors) to WIN; +2 (so rock) to LOSE;
        let (needed_outcome, outcome_shift) = match line_bytes[2] {
            b'X' => (Outcome::LOSE, 2),
            b'Y' => (Outcome::DRAW, 0),
            b'Z' => (Outcome::WIN, 1),
            _ => panic!("Unexpected outcome!"),
        };

        // opponent - 1 to make it 0-based
        let needed_me = 1 + (outcome_shift + opponent - 1) % 3;
        points2 += needed_outcome as i32 + needed_me as i32;
    }

    println!("Part1: Total points are {}", points);
    println!("Part2: Total points are {}", points2);

    Ok(())
}
