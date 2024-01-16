// run with `cargo run --bin d16` (in dir with Cargo.toml)
use std::vec;
use std::fs;
use std::io;
use std::collections::HashMap;
use std::collections::HashSet;
use std::cmp::max;

// #[derive(Debug)]
// #[repr(u8)]
// enum CellKind {
//     Space = b'.',
//     MirrorA = b'/',
//     MirrorB = b'\\',
//     SplitterVertical = b'|',
//     SplitterHorizontal = b'-',
// }

pub fn main() -> Result<(), io::Error> {
    let input = fs::read_to_string("../d16_input.txt")?;
    let mut grid: Vec<Vec<u8>> = vec!();
    for (_y, line) in input.split('\n').enumerate() {
        if line.len() == 0 {
            continue;
        }
        let mut grid_line = vec!();
        for (_x, c) in line.bytes().enumerate() {
            // was not worth it
            // grid_line.push(match c {
            //     b'.'  => CellKind::Space,
            //     b'/'  => CellKind::MirrorA,
            //     b'\\' => CellKind::MirrorB,
            //     b'|'  => CellKind::SplitterVertical,
            //     b'-'  => CellKind::SplitterHorizontal,
            //     _ => panic!("unmatched cellkind"),
            // })
            grid_line.push(c);
        }

        grid.push(grid_line);
    }

    let num_energized = count_energized(&grid, (0, 0), (0, 1));
    println!("Part1: {}", num_energized);

    // brute force
    let max_y = grid.len() as i32 - 1;
    let max_x = grid[0].len() as i32 - 1;
    let mut max_energized = 0;
    for y in 0..=max_y {
        if y == 0 || y == max_y {
            // handle columns up/down
            let direction = if y == 0 {
                // down if coming from top row
                (1, 0)
            } else {
                (-1, 0)
            };

            for x in 0..=max_x {
                let num_energized = count_energized(&grid, (y, x), direction);
                max_energized = max(max_energized, num_energized);
            }
        }

        // handle rows left/right
        // this also handles the corners on top/bottom row
        let num_energized_left = count_energized(&grid, (y, 0), (0, 1));
        let num_energized_right = count_energized(&grid, (y, max_x), (0, -1));
        max_energized = max(max_energized,
                            max(num_energized_left, num_energized_right));
    }

    println!("Part2: {}", max_energized);
    Ok(())
}

fn count_energized(
    grid: &Vec<Vec<u8>>,
    start: (i32, i32),
    start_dir: (i32, i32)
) -> usize {
    let max_y = grid.len() - 1;
    let max_x = grid[0].len() - 1;
    let mut energized: HashMap<(i32, i32), bool> = HashMap::new();
    let mut visited: HashSet<((i32, i32), (i32, i32))> = HashSet::new();
    let mut stack = vec!((start, start_dir));
    while stack.len() > 0 {
        let ((y, x), (dy, dx)) = stack.pop().unwrap();

        // out of bounds or visited (with same direction)
        if y < 0 || x < 0 || y as usize > max_y || x as usize > max_x 
           || visited.contains(&((y, x), (dy, dx))) {
            continue
        }
        // mark as energized
        // NOTE: too lazy to switch to HashSet and might need a map later
        energized.insert((y, x), true);
        visited.insert(((y, x), (dy, dx)));

        let cell = grid[y as usize][x as usize];
        let (dy, dx) = match (cell, (dy, dx)) {
            (b'.', _) => (dy, dx),
            // // right -> up
            // b'/', (0, 1) => (-1, 0),
            // // down -> left
            // b'/', (1, 0) => (0, -1),
            // b'/', (0, -1) => (1, 0),
            // b'/', (-1, 0) => (0, 1),
            // => (-dx, -dy)
            (b'/', (dy, dx)) => (-dx, -dy),

            // b'\\', (0, 1) => (1, 0),
            // b'\\', (1, 0) => (0, 1),
            // b'\\', (0, -1) => (-1, 0),
            // b'\\', (-1, 0) => (0, -1),
            // => swapping (dx, dy)
            (b'\\', (dy, dx)) => (dx, dy),

            // up/down just passes through
            (b'|', (_, 0)) => (dy, dx),
            // left/right passes through
            (b'-', (0, _)) => (dy, dx),

            // split
            (b'|', (0, _)) => {
                // queue down
                stack.push(((y + 1, x), (1, 0)));
                // we continue up
                (-1, 0)
            },
            (b'-', (_, 0)) => {
                // queue left
                stack.push(((y, x - 1), (0, -1)));
                // we continue right
                (0, 1)
            },
            _ => panic!("unhandled combination"),
        };

        stack.push(((y + dy, x + dx), (dy, dx)));
    }

    let num_energized = energized.len();
    num_energized
}
