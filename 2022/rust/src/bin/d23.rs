use std::io;
use std::fs;
use std::collections::HashSet;

const USE_EXAMPLE: bool = false;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Pos {
    row: i16,
    col: i16,
}

enum Neighbour {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}

// take iter, which is an I
// where I implements IntoIterator with the associated type Pos
fn pos_min_max<'a, I>(iter: I) -> (Pos, Pos) 
    where
        I: std::iter::IntoIterator<Item = &'a Pos>
{
    let mut min = Pos{ row: i16::MAX, col: i16::MAX };
    let mut max = Pos{ row: 0, col: 0 };
    for elve in iter {
        if elve.row > max.row {
            max.row = elve.row;
        } else if elve.row < min.row {
            min.row = elve.row;
        }
        if elve.col > max.col {
            max.col = elve.col;
        } else if elve.col < min.col {
            min.col = elve.col;
        }
    }

    (min, max)
}

fn print_grid(elves: &HashSet<Pos>) {
    let (min, max) = pos_min_max(elves.into_iter());
    for row in min.row..=max.row {
        for col in min.col..=max.col {
            if elves.contains(&Pos{ row, col }) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!("");
    }
}

fn sim_round(elves: &mut HashSet<Pos>, round: usize, rules: &[([Pos; 3], Pos); 4], offsets: &[Pos; 8]) -> bool {
    // let mut proposed = HashSet::new();
    let mut moved = false;
    let mut proposed: Vec<(Pos, Pos, bool)> = vec!();
    for pos in elves.iter() {
        if offsets
                .iter()
                .all(|&off| !elves.contains(&Pos{ row: pos.row + off.row, col: pos.col + off.col })) {
            // no elves in any of the adjacent fields
            continue;
        }
        for i in 0..4 {
            // each round moves starting rule by one
            let (neighbours, proposal) = &rules[(i + round) % 4];
            let mut valid = true;
            for offset in neighbours {
                let test = Pos{ row: pos.row + offset.row, col: pos.col + offset.col };
                if elves.contains(&test) {
                    valid = false;
                    break;
                }
            }

            if valid {
                // add proposal and end elves first round half
                let new_pos = Pos{ row: pos.row + proposal.row, col: pos.col + proposal.col };
                // could not figure out how to return a mutable ref from this and assign to it
                let mb_pre_existing = proposed.iter_mut().find(|(_, np, _)| np == &new_pos);
                if let Some(pre_existing) = mb_pre_existing {
                    *pre_existing = (pre_existing.0, pre_existing.1, false);
                } else {
                    proposed.push((*pos, new_pos, true));
                }
                break;
            }
        }
    }

    // do proposed moves if unique
    for (prev, new, unique) in proposed {
        if !unique { continue; }
        elves.remove(&prev);
        elves.insert(new);
        moved = true;
    }

    moved
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string(if USE_EXAMPLE { "../d23.example.in" } else { "../d23.in" })?;
    let contents = contents.trim();
    let mut elves_inital = HashSet::new();
    for (row, line) in contents.lines().enumerate() {
        for (col, c) in line.bytes().enumerate() {
            match c {
                b'#' => { elves_inital.insert(Pos { row: row as i16, col: col as i16 }); },
                _ => {},
            };
        }
    }

    let offsets = [
        // up
        Pos{ row: -1, col:  0 }, 
        // upright
        Pos{ row: -1, col:  1 }, 
        // right
        Pos{ row:  0, col:  1 }, 
        // downright
        Pos{ row:  1, col:  1 }, 
        // down
        Pos{ row:  1, col:  0 }, 
        // downleft
        Pos{ row:  1, col: -1 }, 
        // left
        Pos{ row:  0, col: -1 }, 
        // upleft
        Pos{ row: -1, col:  -1 }, 
    ];
    let rules = [
        // 3 pos to check -> if empty -> propose move
        ([offsets[Neighbour::N as usize], offsets[Neighbour::NW as usize], offsets[Neighbour::NE as usize]],
         offsets[Neighbour::N as usize]),
        ([offsets[Neighbour::S as usize], offsets[Neighbour::SW as usize], offsets[Neighbour::SE as usize]],
         offsets[Neighbour::S as usize]),
        ([offsets[Neighbour::W as usize], offsets[Neighbour::NW as usize], offsets[Neighbour::SW as usize]],
         offsets[Neighbour::W as usize]),
        ([offsets[Neighbour::E as usize], offsets[Neighbour::NE as usize], offsets[Neighbour::SE as usize]],
         offsets[Neighbour::E as usize]),
    ];

    let mut round = 0;
    let mut elves = elves_inital.clone();
    while round < 10 {
        sim_round(&mut elves, round, &rules, &offsets);
        // println!("After Round {}", round + 1);
        // print_grid(&elves);
        round += 1;
    }

    let (min, max) = pos_min_max(elves.iter());
    let width = max.col - min.col + 1;
    let height = max.row - min.row + 1;
    let ground_fields = width * height - elves.len() as i16;
    println!("Height {} Width {} Elves {}", height, width, elves.len());
    println!("Part1: Empty ground fields after 10 rounds: {}", ground_fields);

    while sim_round(&mut elves, round, &rules, &offsets) {
        round += 1;
    }
    println!("Part2: Stopped moving after round {}", round + 1);
    Ok(())
}
