use std::fs;
use std::io;

struct Range {
    start: i32,
    end_inclusive: i32,
}

impl Range {
    pub fn contains(&self, other: &Range) -> bool {
        if self.start <= other.start &&
                self.start <= other.end_inclusive &&
                self.end_inclusive >= other.end_inclusive {
            true
        } else {
            false
        }
    }

    pub fn overlaps(&self, other: &Range) -> bool {
        if self.start > other.end_inclusive {
            // starts after other
            false
        } else if self.start < other.start && self.end_inclusive < other.start {
            // start+ends before other
            false
        } else {
            true
        }
    }
}

fn range_from_str(s: &str) -> Range {
    let dash_idx = s.find("-").unwrap();
    Range {
        start: s[..dash_idx].parse::<i32>().unwrap(),
        end_inclusive: s[dash_idx + 1..].parse::<i32>().unwrap(),
    }
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d04.in")?;

    let mut fully_contained = 0;
    let mut overlaps = 0;
    for line in contents.split('\n') {
        if line == "" { continue };
        // println!("{}", line);

        let comma_idx = line.find(",").unwrap();
        let range1 = &line[..comma_idx];
        let range2 = &line[comma_idx + 1..];
        let range1 = range_from_str(range1);
        let range2 = range_from_str(range2);
        if range1.contains(&range2) || range2.contains(&range1) {
            fully_contained += 1;
        }
        if range1.overlaps(&range2) {
            overlaps += 1;
        }
    }

    println!("Part1: Fully contained ranges {}", fully_contained);
    println!("Part2: Overlapping ranges {}", overlaps);
    Ok(())
}
