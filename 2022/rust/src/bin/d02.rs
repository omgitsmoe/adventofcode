use std::fs;
use std::io;

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d02.in")?;

    let mut points = 0;
    let mut points2 = 0;
    for line in contents.split('\n') {
        if line == "" { continue };
        // println!("{}", line);

        let line_bytes = line.as_bytes();
        let opponent = (line_bytes[0] - b'A') as i32;
        let me = (line_bytes[2] - b'X') as i32;
        // see cpp for explanation
        // rem_euclid won't have negative outcomes when doing 'modulo'
        let outcome = (me - opponent + 1).rem_euclid(3);
        points += outcome * 3;
        points += me + 1;

        // part2
        let needed_outcome = me;
        let needed_me = (opponent - 1 + needed_outcome).rem_euclid(3);
        points2 += needed_outcome * 3 + needed_me + 1;
    }

    println!("Part1: Total points are {}", points);
    println!("Part2: Total points are {}", points2);

    Ok(())
}
