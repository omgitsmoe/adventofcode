use std::io;
use std::fs;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct Point {
    y: i32,
    x: i32,
}

#[derive(Debug, Clone)]
struct Node {
    height: i8,
    path_cost: i32,
    visited: bool,
    pos: Point,
    prev: Point,
}

macro_rules! index {
    ( $y:expr, $x:expr, $cols:ident ) => {
        $y as usize * $cols + $x as usize
    }
}

fn dijkstra(mut grid: Vec<Node>, start: Point, end: Point, cols: usize) -> usize {
    // need to use a block/scope to end the mutable borrow
    {
        let start: &mut Node = &mut grid[index!(start.y, start.x, cols)];
        start.visited = true;
        start.path_cost = 0;
    }

    let rows = grid.len() / cols;
    let neighbours = 
        [Point { y: -1, x: 0 }, Point { y: 1, x: 0 }, Point { y: 0, x: 1 }, Point { y: 0, x: -1 }];

    let mut q = VecDeque::from([start]);
    while !q.is_empty() {
        // pick node with the lowest path_cost
        // front as starting min
        let mut min = match q.front().unwrap() {
            Point {y, x} => grid[index!(*y, *x, cols)].path_cost,
        };
        let mut min_idx = 0;
        for (i, p) in q.iter().enumerate() {
            let node = &grid[index!(p.y, p.x, cols)];
            if node.path_cost < min {
                min = node.path_cost;
                min_idx = i;
            }
        }

        let current = q.remove(min_idx).unwrap();
        let node_height;
        let node_path_cost;
        {
            // cannot keep the immutable borrow here
            let node = &grid[index!(current.y, current.x, cols)];
            // so chache the needed values
            node_height = node.height;
            node_path_cost = node.path_cost;
        }

        for n in &neighbours {
            let next = Point { y: current.y + n.y, x: current.x + n.x };
            if next.y < 0 || next.y as usize >= rows || next.x < 0 || next.x as usize >= cols {
                // out of range
                continue;
            }

            let next_node = &mut grid[index!(next.y, next.x, cols)];
            if (next_node.height - node_height) > 1 {
                // elevation too high
                continue;
            }
            if node_path_cost + 1 < next_node.path_cost {
                next_node.path_cost = node_path_cost + 1;
                next_node.prev = current.clone();
            }
            if !next_node.visited {
                q.push_back(next_node.pos.clone());
                next_node.visited = true;
            }
        }
    }

    grid[index!(end.y, end.x, cols)].path_cost as usize
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d12.in")?;
    let contents = contents.trim();
    let mut grid = Vec::new();
    let mut start = Point { y: 0, x: 0 };
    let mut end = Point { y: 0, x: 0 };
    let mut cols = 0;
    for (y, line) in contents.lines().enumerate() {
        cols = line.len();
        for (x, b) in line.bytes().enumerate() {
            let height;
            let pos = Point{ y: y as i32, x: x as i32 };
            match b {
                b'S' => {
                    start = pos.clone();
                    // real height
                    height = 0;
                },
                b'E' => {
                    end = pos.clone();
                    // real height
                    height = b'z' - b'a';
                },
                _ => {
                    height = b - b'a';
                },
            }

            grid.push(Node{
                height: height as i8,
                path_cost: i32::MAX,
                visited: false,
                pos,
                prev: Point{ y: -1, x: -1 }
            });
        }
    }

    let steps1 = dijkstra(grid.clone(), start, end.clone(), cols);
    println!("Part1: Lowest amount of steps required: {}", steps1);

    let mut steps2 = usize::MAX;
    for node in &grid {
        if node.height == 0 {
            let steps = dijkstra(grid.clone(), node.pos.clone(), end.clone(), cols);
            if steps < steps2 {
                steps2 = steps;
            }
        }
    }
    println!("Part2: Lowest amount of steps required: {}", steps2);

    Ok(())
}
