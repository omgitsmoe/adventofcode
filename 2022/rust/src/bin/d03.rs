use std::fs;
use std::io;

fn mark_items(items: &mut [bool], s: &str) {
    for c in s.as_bytes() {
        let idx = if *c <= b'Z' { c - b'A' + 26 } else { c - b'a' };
        items[idx as usize] = true;
    }
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d03.in")?;

    let mut point_sum = 0;
    for line in contents.split('\n') {
        if line == "" { continue };
        // println!("{}", line);

        let half = line.len() / 2;
        let (compartment1, compartment2) = line.split_at(half);
        // no std bitset :/
        let mut items1 = [false; 53];
        mark_items(&mut items1[..], compartment1);
        let mut items2 = [false; 53];
        mark_items(&mut items2[..], compartment2);
        for i in 0..items1.len() {
            let in_both = items1[i] & items2[i];
            if in_both {
                point_sum += i + 1;
            }
        }
    }

    println!("Part1: Duplicate items prios are {}", point_sum);

    let mut common_point_sum = 0;
    let mut rucksacks = [
        [false; 53],
        [false; 53],
        [false; 53],
    ];
    // so we don't sum up instantly -> start at 1
    let mut idx = 1;
    for line in contents.split('\n') {
        if line == "" { continue };
        // println!("{}", line);

        let items = &mut rucksacks[idx % 3];
        mark_items(&mut items[..], line);

        if idx % 3 == 0 {
            for i in 0..rucksacks[0].len() {
                let in_all = rucksacks[0][i] & rucksacks[1][i] & rucksacks[2][i];
                if in_all {
                    common_point_sum += i + 1;
                }
            }
            // reset
            rucksacks = [
                [false; 53],
                [false; 53],
                [false; 53],
            ];
        }

        idx += 1;
    }

    println!("Part2: Common item prios are {}", common_point_sum);

    Ok(())
}
