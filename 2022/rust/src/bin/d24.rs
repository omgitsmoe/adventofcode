use std::io;
use std::fs;
use std::ops;
use std::cmp;
use std::str::FromStr;
use std::collections::VecDeque;
// use std::collections::BinaryHeap;
use std::collections::HashSet;
use std::collections::HashMap;

const USE_EXAMPLE: bool = false;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
struct Pos {
    row: i16,
    col: i16,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl FromStr for Direction {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "^" => Ok(Direction::Up),
            ">" => Ok(Direction::Right),
            "v" => Ok(Direction::Down),
            "<" => Ok(Direction::Left),
            _ => Err(()),
        }
    }
}

impl ops::Add<Pos> for Pos {
    type Output = Pos;
    fn add(self, rhs: Pos) -> Self::Output {
        Pos { row: self.row + rhs.row, col: self.col + rhs.col }
    }
}

impl ops::Mul<i16> for Pos {
    type Output = Self;
    fn mul(self, rhs: i16) -> Self::Output {
        Pos{ row: self.row * rhs, col: self.col * rhs }
    }
}

// instead of 4-tuple, so it's usable with the BinaryHeap
#[derive(PartialEq, Clone, Debug)]
struct State {
    pos: Pos,
    minute: usize,
    dist_per_minute: f32,
}

// can't derive, due to f32
impl Eq for State {}

// The priority queue depends on `Ord`.
// NOTE: couldn't implement this for tuple we had, since we don't "own" the type or trait
// NOTE: no Ord for f32, so use partial_cmp inside it
// using other.partial_cmp... would make it a min-heap
// self.partial_cmp max-heap
impl cmp::Ord for State {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        // compare the float
        self.dist_per_minute.partial_cmp(&other.dist_per_minute)
    }
}


// NOTE: not caching this was the major slowdowns (5min +), was caching it previously, buth for 
// that solution it was actually slower -.-
fn get_state<'a>(
    cache: &'a mut Vec<HashSet<Pos>>,
    start: &Vec<(Pos, Direction)>,
    time: usize,
    max: &Pos,
) -> &'a HashSet<Pos> {
    if time >= cache.len() {
        let neighbours = [
            // up
            Pos{ row: -1, col:  0 },
            // right
            Pos{ row:  0, col:  1 },
            // down
            Pos{ row:  1, col:  0 },
            // left
            Pos{ row:  0, col: -1 },
        ];
        // NOTE: assumes cache[0]=starting state
        // generate missing states up to and including time
        for t in cache.len()..=time {
            // sim to t in one step
            // since blizzards don't interact with one another
            let mut at_t = HashSet::new();
            for (p, dir) in start {
                let single = neighbours[*dir as usize];

                // wrap t first, see below
                // (still needed below since we might still into a wall etc)
                // isize due to possible overflow
                let off_row = (single.row as isize * ((t - 1) % (max.row as usize - 1) + 1) as isize) as i16;
                let off_col = (single.col as isize * ((t - 1) % (max.col as usize - 1) + 1) as isize) as i16;
                // let test = *p + off;

                // wrap using modulo
                // since 0 and max.row/col are walls we need to bias row/col to 0..<max.row/col
                // then modulo with the decreased range and then add one to bring it to it's original range
                // so we get 1..<max.row/col
                // % in rust is the __remainder__ -> use rem_euclid to get modulo for neg numbers
                let new = Pos{
                    row: ((p.row + off_row - 1).rem_euclid(max.row - 1) + 1) as i16,
                    col: ((p.col + off_col - 1).rem_euclid(max.col - 1) + 1) as i16,
                };

                // lookup key new, if not present insert vec!() on result
                // (either previous entry or empty vec) add dir
                // next.entry(new).or_insert(vec!()).push(*dir);
                at_t.insert(new);
            }

            cache.push(at_t);
        }
    }

    &cache[time]
}

fn print_grid(state: HashSet<Pos>, max: &Pos) {
    for row in 0..max.row+1 {
        for col in 0..max.col+1 {
            if row == 0 || col == 0 || row == max.row || col == max.col {
                print!("#");
            } else if state.contains(&Pos{ row, col }) {
                print!("B");
            } else {
                print!(".");
            }
        }
        println!("");
    }
    println!("");
}

// NOTE: no speed gain over the caching solution (cached sets: ~300ms vs. pos offsets ~300ms)
// very good explanation by u/segloff23
// We can avoid any sort of simulations or caching, and instead index directly into 4 different
// mask arrays. For example, on the test input the > mask would look like this:
//
// #.######      #.######
// #>>.<^<#      #>>....#
// #.<..<<#   => #......#
// #>v.><>#      #>..>.>#
// #<^v^^>#      #.....>#
// ######.#      ######.#
//
// If a cell (r, c) is to be occupied at time t by a right moving blizzard, than there must have
// been a blizzard t cells to the left, modulo our grid width since they wrap: (r, (c-t) % C). The
// same lookup can be applied in each direction.
//
// You can also use only one array, and check if grid[(r, (c-t) % C)] is equal to ">", I'm just
// precomputing this inequality. I actually found that with my implementation of A*, the caching
// the neighbors using lcm(R, C) had no performance impact when using this approach.
fn check_blizzard(start: &HashMap<Pos, Direction>, max: Pos, pos: Pos, t: usize) -> bool {
    let neighbours = [
        // up
        Pos{ row: -1, col:  0 },
        // right
        Pos{ row:  0, col:  1 },
        // down
        Pos{ row:  1, col:  0 },
        // left
        Pos{ row:  0, col: -1 },
    ];
    // NOTE: assumes pos is valid (inside grid)
    // NOTE: apparently you can also check the input grid position offset by minute modulo gridsize
    // to check if there will be a blizzard u/paul_sb76

    for dir in [Direction::Up, Direction::Right, Direction::Down, Direction::Left] {
        // need to test each blizzard direction separately
        // we need to shift the position we're testing by the __opposite__ direction that
        // the blizzard is traveling in (or rather the dir we're currently testing)
        let off = neighbours[(dir as usize + 2) % 4];
        // NOTE: shift the position we're testing in the opposite direction of the blizzard's
        // travel by the time step
        // wrap using modulo
        // since 0 and max.row/col are walls we need to bias row/col to 0..<max.row/col
        // then modulo with the decreased range and then add one to bring it to it's original range
        // so we get 1..<max.row/col
        // % in rust is the __remainder__ -> use rem_euclid to get modulo for neg numbers
        // NOTE: would be much easier if I excluded the walls from the grid
        // then we could just use % width or % height
        // e.g. row = (pos.row + off.row * t).rem_euclid(height)
        let test = Pos{
            row: ((pos.row as isize + off.row as isize * t as isize - 1)
                  .rem_euclid(max.row as isize - 1) + 1) as i16,
            col: ((pos.col as isize + off.col as isize * t as isize - 1)
                  .rem_euclid(max.col as isize - 1) + 1) as i16,
        };

        // check if we hit a blizzard that has the direction that we're __currently__ testing
        // alternative is to split the blizzards up into different sets, one for each direction
        match start.get(&test) {
            Some(found_dir) => if &dir == found_dir { return true },
            None => continue,
        }
    }

    false
}

fn minutes_till_goal(
    blizzards: &HashMap<Pos, Direction>,
    max: Pos,
    start_pos: Pos,
    start_t: usize,
    goal: Pos,
) -> usize {
    let options = [
        // // wait
        // Pos{ row:  0, col:  0 },
        // up
        Pos{ row: -1, col:  0 },
        // right
        Pos{ row:  0, col:  1 },
        // down
        Pos{ row:  1, col:  0 },
        // left
        Pos{ row:  0, col: -1 },
    ];
    // BFS will find shortest path in unweighted "graph"
    let mut q = VecDeque::new();
    // NOTE: tried using a prio q with dist travelled per minute, since the bfs version
    // was so slow, but it turned out to be other things (like generating valley_at_t in
    // one step for the current on demand without caching [removed caching for it since for
    // the solution __at the time__ it was faster??], or keeping a separate pos visited
    // set per q item) that made that slow and the BFS is faster (~100ms) vs the prio q (~500ms)
    // let mut q = BinaryHeap::new();
    q.push_back((start_pos, start_t));
    let mut seen = HashSet::new();
    let mut minutes_taken = 0;
    'outer: while !q.is_empty() {
        let (pos, minute) = q.pop_front().unwrap();
        // println!("Pos {:?} Minute {}", pos, minute);

        // consider all possible moves
        for off in options {
            let new_pos = pos + off;
            if seen.contains(&(new_pos, minute + 1)) {
                continue;
            } else if new_pos == goal {
                // otherwise we can't q waiting or hitting the goal
                // reached exit
                minutes_taken = minute + 1 - start_t;
                break 'outer;
            } else if new_pos.row <= 0 || new_pos.row >= max.row || 
                    new_pos.col <= 0 || new_pos.col >= max.col {
                // hit wall
                continue;
            } else if check_blizzard(&blizzards, max, new_pos, minute + 1) {
                // occupied by blizzard
                continue;
            } else {
                // let dist_rem = dist_total - goal.row - new_pos.row + goal.col - new_pos.col;
                q.push_back((new_pos, minute + 1));
                // NOTE: was previously saving all visited pos, since using a global set for visited
                // positions didn't work BUT it only didn't work since I din't insert right after queueing
                // but at popping time -> q already exploded since I was using BFS
                // (now, since it's global also need to track time)
                seen.insert((new_pos, minute + 1));
            }
        }
        // wait
        if !check_blizzard(&blizzards, max, pos, minute + 1) {
            // let dist_rem = dist_total - goal.row - state.pos.row + goal.col - state.pos.col;
            q.push_back((pos, minute + 1));
            seen.insert((pos, minute + 1));
        }
    }

    minutes_taken
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string(if USE_EXAMPLE { "../d24.example.in" } else { "../d24.in" })?;
    let contents = contents.trim();

    let mut max_row = 0;
    let mut max_col = 0;
    let mut blizzards = vec!();
    for (row, line) in contents.lines().enumerate() {
        if row > max_row {
            max_row = row;
        }
        for (col, b) in line.bytes().enumerate() {
            if col > max_col {
                max_col = col;
            }
            blizzards.push(
                (
                    Pos{ row: row as i16, col: col as i16 },
                    match b {
                        b'^' => Direction::Up,
                        b'>' => Direction::Right,
                        b'v' => Direction::Down,
                        b'<' => Direction::Left,
                        _ => continue,
                    }
                )
            );
        }
    }

    let max = Pos{ row: max_row as i16, col: max_col as i16 };
    println!("Walls {:?}", max);
    let start_pos = Pos{ row: 0, col: 1 };
    let goal = Pos { row: max_row as i16, col: max_col as i16 - 1 };
    // let dist_total = goal.row - start_pos.row + goal.col - start_pos.col;

    let blizzard_start =
        blizzards
        .iter()
        .map(|(p, dir)| (*p, *dir))
        .collect::<HashMap<_,_>>();

    // NOTE: another optimization would be that the state of the grid repeats itself
    // the amount of steps that are needed to cycle corresponds to the least common multiple (lcm)
    // of the width and height
    // (if all horizonntal blizzards travelled exactly width steps and all vertical blizzards
    //  travelled exactly height steps, then they will be at their original position)
    // -> we would mod the minute in our seen set by the lcm
    // (which is lcm = height * width / gcd(height, width))
    // -> skip more states
    // credit: u/hyper-neutrino
    let t_first = minutes_till_goal(&blizzard_start, max, start_pos, 0, goal);
    println!("Part1: Reached exit after {} minutes", t_first);
    // back to start
    let t_second = minutes_till_goal(&blizzard_start, max, goal, t_first, start_pos);
    // back to goal again
    let t_third = minutes_till_goal(&blizzard_start, max, start_pos, t_first + t_second, goal);
    println!("Part2: Brought snacks back in {} minutes", t_first + t_second + t_third);

    Ok(())
}
