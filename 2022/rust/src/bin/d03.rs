use std::fs;
use std::io;

fn bits(s: &str) -> u64 {
    let mut result = 0u64;
    for c in s.as_bytes() {
        let idx = if *c <= b'Z' { c - b'A' + 26 } else { c - b'a' };
        result |= 1 << idx;
    }

    result
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d03.in")?;

    let mut point_sum = 0;
    for line in contents.split('\n') {
        if line == "" { continue };
        // println!("{}", line);

        let half = line.len() / 2;
        let (compartment1, compartment2) = line.split_at(half);
        let items1 = bits(compartment1);
        let items2 = bits(compartment2);
        // assumes there's only ONE active bit or duplicate item between compartments
        let common = items1 & items2;
        // get index of active bit
        point_sum += u64::BITS - common.leading_zeros();
    }

    println!("Part1: Duplicate items prios are {}", point_sum);

    let mut common_point_sum = 0;
    let mut iter = contents.lines().peekable();
    while iter.peek().is_some()  {
        let items1 = bits(iter.next().unwrap());
        let items2 = bits(iter.next().unwrap());
        let items3 = bits(iter.next().unwrap());
        let common = items1 & items2 & items3;
        common_point_sum += u64::BITS - common.leading_zeros();
    }

    println!("Part2: Common item prios are {}", common_point_sum);

    Ok(())
}
