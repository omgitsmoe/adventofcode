use std::vec;
use std::fs;
use std::io;

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d01.in")?;
    let mut elves = vec!();

    let mut cursum = 0;
    let mut curmax = 0;
    let mut curmax_idx = 0;
    for (i, line) in contents.split('\n').enumerate() {
        if line == "" {
            if cursum > curmax {
                curmax = cursum;
                curmax_idx = i;
            }
            elves.push(cursum);
            cursum = 0;
        } else {
            let rations: i32 = line.parse().expect("Unexpected line");
            cursum += rations;
        }
        println!("{}:{}", i, line);
    }

    println!("Elve {} has {} rations!", curmax_idx + 1, curmax);

    elves.sort();

    println!("The top 3 elves have {} rations!",
             elves.into_iter().rev().take(3).fold(0i64, |acc, x| acc + (x as i64)));
    Ok(())
}
