/// Rust 1397 / 688
/// 
/// Run Time
/// Part 1: 2.8ms
/// Part 2: 4.8ms
/// 
/// Source
/// 
/// https://github.com/dclamage/AOC2022/blob/main/day17/src/main.rs
/// 
/// Description
/// 
/// As usual for AoC "maps" I didn't allocate a full array for the "chamber" and instead used a HashSet of points that are occupied. The HashSet is not updated until a shape "settles", at which point the max height seen is also updated.
/// 
/// Part 1 was just about following the simulation properly. Creating a function to print the "chamber" was very helpful for finding my off-by-one errors and such. It took me about an hour to implement it properly, and most of that time was spent debugging why my answer was different from the example. There were a few different issues, including my manual transcription of the shapes into offsets being incorrect which I didn't catch until I printed them as I stepped.
/// 
/// Part 2 would have taken 24 days to run using a naïve simulation as in part 1. Given that I didn't want to run it for 24 days, I figured that since the shapes follow a cycle and the "gusts" also follow a cycle, perhaps the simulation does as well. And given that it's AoC and an answer should be possible, this seemed quite likely. There may be a way to actually mathematically determine this, but instead I did the following:
/// 
///     Simulate some large number of shapes settling, but not so many that it takes a long time to do so (final number: 5000)
///     After each shape settles, record the height delta that occurred in a vector.
///     Once the simulation finishes, find a repeating pattern in the height deltas. I did this by looking at cycle lengths from 1 to half the size of the buffer and checking if that cycle repeats for the whole buffer.
///     After finding no cycles even with large simulation sizes, I figured maybe there was some "build up" time before the cycles starts. So I started at a large offset into the buffer and then from there looked for cycles. (Final offset: 250)
///     This found the cycle, so then it was just a matter of summing the initial deltas that were skipped, summing N cycles to almost reach the desired number of shapes, and then partially summing the cycle to reach the exact number of shapes desired.
/// 
/// And that's it. Final run time is only slightly longer than part 1 because I had to simulate 5000 shapes instead of 2022 and of course hunt for the pattern.

use itertools::Itertools;
use std::collections::HashSet;
use std::io::{self, Write};
use std::time::*;
use utility::*;

fn main() {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    // Parsing
    writeln!(stdout, "Parsing...").unwrap();
    let start_time = Instant::now();
    let file_lines = read_file_lines("day17/input.txt");
    //let file_lines = read_file_lines("day17/example-input.txt");
    let elapsed = start_time.elapsed();
    writeln!(stdout, "Parsing time: {}us\n", elapsed.as_micros()).unwrap();

    // Part 1
    writeln!(stdout, "*********** PART 1 ***********").unwrap();
    let start_time = Instant::now();
    let part1_answer = part1(&file_lines);
    let elapsed = start_time.elapsed();
    writeln!(stdout, "Part 1 answer: {}", part1_answer).unwrap();
    writeln!(stdout, "Part 1 time: {}us\n", elapsed.as_micros()).unwrap();

    // Part 2
    writeln!(stdout, "*********** PART 2 ***********").unwrap();
    let start_time = Instant::now();
    let part2_answer = part2(&file_lines);
    let elapsed = start_time.elapsed();
    writeln!(stdout, "Part 2 answer: {}", part2_answer).unwrap();
    writeln!(stdout, "Part 2 time: {}us", elapsed.as_micros()).unwrap();
}

const DEBUG_PRINT: bool = false;
const CHAMBER_WIDTH: i32 = 7;
const SPAWN_X: i32 = 2;
const SPAWN_Y: i32 = 3;
const SHAPES: [(usize, [(i32, i32); 5]); 5] = {
    [
        (4, [(0, 0), (1, 0), (2, 0), (3, 0), (0, 0)]),
        (5, [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]),
        (5, [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]),
        (4, [(0, 0), (0, 1), (0, 2), (0, 3), (0, 0)]),
        (4, [(0, 0), (0, 1), (1, 0), (1, 1), (0, 0)]),
    ]
};

struct Chamber {
    occupied: HashSet<(i32, i32)>,
    last_shape_spawned: Vec<(i32, i32)>,
    movements: Vec<i8>,
    next_movement: usize,
    down_next: bool,
    next_shape: usize,
    highest_y: i32,
    num_stopped_rocks: usize,
}

impl Chamber {
    fn new(line: &str) -> Chamber {
        Chamber {
            occupied: HashSet::new(),
            last_shape_spawned: Vec::new(),
            movements: line
                .chars()
                .map(|c| match c {
                    '<' => -1,
                    '>' => 1,
                    _ => panic!("Invalid movement {}", c),
                })
                .collect_vec(),
            next_movement: 0,
            down_next: false,
            next_shape: 0,
            highest_y: 0,
            num_stopped_rocks: 0,
        }
    }

    fn step(&mut self) {
        if !self.last_shape_spawned.is_empty() {
            let movement: (i32, i32) = if self.down_next {
                (0, -1)
            } else {
                let movement = self.movements[self.next_movement] as i32;
                self.next_movement = (self.next_movement + 1) % self.movements.len();
                (movement, 0)
            };
            if DEBUG_PRINT {
                println!("Movement: {:?}", movement);
            }
            let can_move = self.last_shape_spawned.iter().all(|(x, y)| {
                let new_x = x + movement.0;
                let new_y = y + movement.1;
                (0..CHAMBER_WIDTH).contains(&new_x)
                    && new_y > 0
                    && !self.occupied.contains(&(new_x, new_y))
            });

            if can_move {
                for (x, y) in self.last_shape_spawned.iter_mut() {
                    *x += movement.0;
                    *y += movement.1;
                }
            } else if self.down_next {
                // Shape settles
                self.num_stopped_rocks += 1;
                for (x, y) in self.last_shape_spawned.iter() {
                    self.occupied.insert((*x, *y));
                    self.highest_y = self.highest_y.max(*y);
                }

                self.last_shape_spawned.clear();
                self.down_next = false;
                return;
            }
            self.down_next = !self.down_next;
        } else {
            // Spawn next shape
            let (shape_size, shape) = SHAPES[self.next_shape];
            self.next_shape = (self.next_shape + 1) % SHAPES.len();
            let mut shape_spawned = Vec::new();

            let base_y = self.highest_y + SPAWN_Y + 1;
            if DEBUG_PRINT {
                println!("Base y: {}", base_y);
            }
            for (x, y) in shape.iter().take(shape_size) {
                let new_x = x + SPAWN_X;
                let new_y = base_y + y;
                shape_spawned.push((new_x, new_y));
            }
            self.last_shape_spawned = shape_spawned;
        }
    }

    #[allow(dead_code)]
    fn print(&self) {
        let max_y = self.highest_y.max(
            self.last_shape_spawned
                .iter()
                .copied()
                .map(|(_, y)| y)
                .max()
                .unwrap_or(0),
        );
        for y in (0..=max_y).rev() {
            for x in -1..CHAMBER_WIDTH + 1 {
                if y == 0 {
                    if x == -1 || x == CHAMBER_WIDTH {
                        print!("+");
                    } else {
                        print!("-");
                    }
                } else if x == -1 || x == CHAMBER_WIDTH {
                    print!("|");
                } else if self.occupied.contains(&(x, y)) {
                    print!("#");
                } else if self.last_shape_spawned.contains(&(x, y)) {
                    print!("O");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }
}

fn part1(file_lines: &[String]) -> String {
    let mut chamber = Chamber::new(&file_lines[0]);
    while chamber.num_stopped_rocks < 2022 {
        let new_spawn = chamber.last_shape_spawned.is_empty();
        chamber.step();
        if DEBUG_PRINT && new_spawn {
            chamber.print();
        }
    }

    chamber.highest_y.to_string()
}

fn part2(file_lines: &[String]) -> String {
    // This was fished out for my specific input
    // If it doesn't work for your input, then you can increase the number of shapes to simulate
    const NUM_SHAPES_TO_SIMULATE: usize = 5000;
    let mut chamber = Chamber::new(&file_lines[0]);
    let mut height_delta: Vec<u64> = Vec::with_capacity(NUM_SHAPES_TO_SIMULATE);
    while chamber.num_stopped_rocks < NUM_SHAPES_TO_SIMULATE {
        let prev_height = chamber.highest_y;
        chamber.step();
        let post_height = chamber.highest_y;
        if chamber.last_shape_spawned.is_empty() {
            height_delta.push((post_height - prev_height) as u64);
        }
    }

    // Find the pattern
    // The initial skip is to avoid the first few shapes which are not part of the pattern
    // This was also fished out for my input. If it doesn't work for your input, then you can
    // increase the number to skip.
    const INITIAL_PATTERN_SKIP_LEN: usize = 250;
    let height_delta_for_pattern = &height_delta[INITIAL_PATTERN_SKIP_LEN..];
    let mut found_pattern_len = 0;
    for pattern_len in 1..=height_delta_for_pattern.len() / 2 {
        let pattern = &height_delta_for_pattern[0..pattern_len];
        let mut found = true;
        for i in 0..height_delta_for_pattern.len() - pattern_len {
            if height_delta_for_pattern[i + pattern_len] != pattern[i % pattern_len] {
                found = false;
                break;
            }
        }
        if found {
            found_pattern_len = pattern_len;
            break;
        }
    }
    assert!(found_pattern_len > 0);

    // Calculate the answer using the found pattern
    const NUM_SHAPES: u64 = 1000000000000;
    let pattern = &height_delta_for_pattern[0..found_pattern_len];
    let pattern_sum = pattern.iter().sum::<u64>();
    let initial_deltas = &height_delta[0..height_delta.len() / 4];
    let initial_sum = initial_deltas.iter().sum::<u64>();
    let num_patterns = (NUM_SHAPES - initial_deltas.len() as u64) / pattern.len() as u64;
    let num_leftover = ((NUM_SHAPES - initial_deltas.len() as u64) % pattern.len() as u64) as usize;
    let leftover_sum = pattern[0..num_leftover].iter().sum::<u64>();

    (initial_sum + pattern_sum * num_patterns + leftover_sum).to_string()
}
