use std::vec;
use std::fs;
use std::io;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
// ^ with dijkstra example, lel
// https://doc.rust-lang.org/std/collections/binary_heap/index.html
use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq, Debug)]
struct State {
    y: u32,
    x: u32,
    dy: i8,
    dx: i8,
    heat_loss: u32,
    path: Vec<(u32, u32)>,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on heat_loss.
        other.heat_loss.cmp(&self.heat_loss)
            // In case of a tie we compare positions - this step is necessary
            // to make implementations of `PartialEq` and `Ord` consistent.
            .then_with(|| (self.y, self.x).cmp(&(other.y, other.x)))
            .then_with(|| (self.dy, self.dx).cmp(&(other.dy, other.dx)))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn main() -> Result<(), io::Error> {
    let input = fs::read_to_string("../d17_input.txt")?;
    let mut grid: Vec<Vec<u8>> = vec!();
    for line in input.split('\n') {
        if line.len() == 0 {
            continue;
        }
        let mut grid_line = vec!();
        for c in line.bytes() {
            let heat_cost = c - b'0';
            grid_line.push(heat_cost);
        }

        grid.push(grid_line);
    }


    // NOTE: edges have not the same cost, so using just BFS does not work!
    // and you need to modify dijkstra/the given grid/(turn it into a proper
    // graph with all the edge information) a bit, see below
    let min_heat_loss = dijkstra(&grid);
    // let min_heat_loss = dijkstra_regular(&grid);
    // let min_heat_loss = dfs(&grid);
    println!("Part1: {}", min_heat_loss);

    let min_heat_loss_part2 = dijkstra2(&grid, 4, 10);
    println!("Part2: {}", min_heat_loss_part2);
    Ok(())
}

// NOTE: regular dijkstra will not work with this graph, since you're only
//       allowed to travel 3 steps in one direction which means that a base
//       assumption of finding the shortest path to a node is violated,
//       since the heat loss score might be lower but due to straight steps
//       left it might still result in a worse path:
//       -> modify the graph to include edges based on direction, straight
//          steps left
//       -> or instead of queueing at most 3 different steps, we queue
//          6 positions one, two, three forward left
//                      one, two, three forward right
// this has same problem: pop State { y: 1, x: 5, dy: 0, dx: 1, heat_loss: 20, path: [(0, 2), (1, 2), (1, 5)] }
// higher loss (0, 5) l23 > 22
// exact same position as well
// -> need to store the shortest path/heat loss with both the position
//    and the direction as key
//    (using the straight moves left as well means we can probably
//    use regular dijkstra as well)
//
fn dijkstra(grid: &Vec<Vec<u8>>) -> u32 {
    let mut heat_loss = HashMap::new();

    // queue that always has path with lowest heat_loss at the front
    let mut queue = BinaryHeap::new();
    let max_y = (grid.len() - 1) as u32;
    let max_x = (grid[0].len() - 1) as u32;
    let start = (0, 0);
    let goal = (max_y, max_x);
    let start_heat_loss = 0;

    // start
    // pos,direction
    heat_loss.insert(((start.0, start.1), (0, 0)), start_heat_loss);
    queue.push(
        // start is special, so use 0,0 as dy,dx so we don't eliminate a
        // direction
        State{y: start.0, x: start.1, dy: 0, dx: 0,
                 heat_loss: start_heat_loss, path: vec!()});

    while let Some(path_info) = queue.pop() {
        // println!("pop {:?}", path_info);
        // reached the goal, with dijkstra this means we also found it using
        // the shortest path, so we can return early
        if (path_info.y, path_info.x) == goal {
            return path_info.heat_loss;
        }

        // we need to check if we already have a path with lower cost
        // at same pos __and__ same direction
        if let Some(cur_min) = heat_loss.get(
                &((path_info.y, path_info.x), (path_info.dy, path_info.dx))) {
            if path_info.heat_loss > *cur_min {
                // already found better path
                continue;
            }
        }

        // iterate over neighbours filtering out directions we can't choose
        // NOTE: instead of queueing single steps deciding when to turn etc.
        //       (see dijkstra_regular for that) we just queue taking a
        //       turn with all possible steps in that direction
        for (ndy, ndx) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
            // can't reverse
            // and we already queue all forward steps
            // -> only allow left/right
            if (ndy, ndx) == (-path_info.dy, -path_info.dx) 
                    || (ndy, ndx) == (path_info.dy, path_info.dx) {
                continue;
            }
            let mut heat_cost = 0;
            // queue all possible forward steps
            for num_steps in 1..=3 {
                let new_y = path_info.y as i32 + ndy as i32 * num_steps;
                let new_x = path_info.x as i32 + ndx as i32 * num_steps;
                if new_y < 0 || new_x < 0 || new_y > max_y as i32 || new_x > max_x as i32 {
                    // out of bounds
                    break;
                }
                let new_y = new_y as u32;
                let new_x = new_x as u32;

                heat_cost += grid[new_y as usize][new_x as usize];
                let next_heat_loss = path_info.heat_loss + heat_cost as u32;
                let cur_min = heat_loss.get(&((new_y, new_x), (ndy, ndx)));
                // NOTE: can't do multiple match arms using the same code
                //       like a fallthrough, kind of
                if cur_min == None || next_heat_loss < *cur_min.unwrap() {
                    // shorter path than existing
                    let mut new_path = path_info.path.clone();
                    new_path.push((new_y, new_x));
                    queue.push(
                        State{
                            y: new_y,
                            x: new_x,
                            dy: ndy, dx: ndx,
                            heat_loss: next_heat_loss,
                            path: new_path});
                    // update cost
                    heat_loss.insert(((new_y, new_x), (ndy, ndx)), next_heat_loss);
                }
            }
        }
    }

    // couldn't find a path
    unreachable!()
}

fn dijkstra2(grid: &Vec<Vec<u8>>, 
             straight_steps_min: u32, 
             straight_steps_max: u32
) -> u32 {
    let mut heat_loss = HashMap::new();

    // queue that always has path with lowest heat_loss and 
    let mut queue = BinaryHeap::new();
    let max_y = (grid.len() - 1) as u32;
    let max_x = (grid[0].len() - 1) as u32;
    let start = (0, 0);
    let goal = (max_y, max_x);
    let start_heat_loss = 0;

    // start
    // pos,direction
    heat_loss.insert(((start.0, start.1), (0, 0)), start_heat_loss);
    queue.push(
        // start is special, so use 0,0 as dy,dx so we don't eliminate a
        // direction
        State{y: start.0, x: start.1, dy: 0, dx: 0,
                 heat_loss: start_heat_loss, path: vec!()});

    while let Some(path_info) = queue.pop() {
        // println!("pop {:?}", path_info);
        if (path_info.y, path_info.x) == goal {
            return path_info.heat_loss;
        }

        // we need to check if we already have a path with lower cost
        // at same pos __and__ same direction
        if let Some(cur_min) = heat_loss.get(&((path_info.y, path_info.x), (path_info.dy, path_info.dx))) {
            if path_info.heat_loss > *cur_min {
                // already found better path
                continue;
            }
        }

        // iterate over neighbours filtering out directions we can't choose
        // NOTE: instead of queueing single steps deciding when to turn etc.
        //       (see dijkstra_regular for that) we just queue taking a
        //       turn with all possible steps in that direction from
        //       straight_steps_min to straight_steps_max
        for (ndy, ndx) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
            // can't reverse
            // and we already queue all forward steps
            // -> only allow left/right
            if (ndy, ndx) == (-path_info.dy, -path_info.dx) 
                    || (ndy, ndx) == (path_info.dy, path_info.dx) {
                continue;
            }
            // in case our straight_steps_min > 1: add the heat cost for all the
            // steps before the minimum
            // NOTE: this could be simplified using an access functin/closure
            let mut heat_cost = (1..straight_steps_min)
                .fold(0, |acc, num_steps| {
                let new_y = path_info.y as i32 + ndy as i32 * num_steps as i32;
                let new_x = path_info.x as i32 + ndx as i32 * num_steps as i32;
                if new_y < 0 || new_x < 0
                    || new_y > max_y as i32 || new_x > max_x as i32 {
                    // out of bounds
                    acc
                } else {
                    acc + grid[new_y as usize][new_x as usize]
                }
            });
            // queue all possible forward steps
            for num_steps in straight_steps_min..=straight_steps_max {
                let num_steps = num_steps as i32;
                let new_y = path_info.y as i32 + ndy as i32 * num_steps;
                let new_x = path_info.x as i32 + ndx as i32 * num_steps;
                if new_y < 0 || new_x < 0 || new_y > max_y as i32 || new_x > max_x as i32 {
                    // out of bounds
                    break;
                }
                let new_y = new_y as u32;
                let new_x = new_x as u32;

                heat_cost += grid[new_y as usize][new_x as usize];
                let next_heat_loss = path_info.heat_loss + heat_cost as u32;
                let cur_min = heat_loss.get(&((new_y, new_x), (ndy, ndx)));
                // NOTE: can't do multiple match arms using the same code
                //       like a fallthrough, kind of
                if cur_min == None || next_heat_loss < *cur_min.unwrap() {
                    // shorter path than existing
                    let mut new_path = path_info.path.clone();
                    new_path.push((new_y, new_x));
                    queue.push(
                        State{
                            y: new_y,
                            x: new_x,
                            dy: ndy, dx: ndx,
                            heat_loss: next_heat_loss,
                            path: new_path});
                    // update cost
                    heat_loss.insert(((new_y, new_x), (ndy, ndx)), next_heat_loss);
                }
            }
        }
    }

    // couldn't find a path
    unreachable!()
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct PathInfo {
    y: u32,
    x: u32,
    dy: i8,
    dx: i8,
    moves_until_turn: u8,
    heat_loss: u32,
    path: Vec<(u32, u32)>,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for PathInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on heat_loss.
        other.heat_loss.cmp(&self.heat_loss)
            // max moves_until_turn, since it may lead to a shorter path
            .then_with(|| self.moves_until_turn.cmp(&other.moves_until_turn))
            // In case of a tie we compare positions - this step is necessary
            // to make implementations of `PartialEq` and `Ord` consistent.
            .then_with(|| (self.y, self.x).cmp(&(other.y, other.x)))
            .then_with(|| (self.dy, self.dx).cmp(&(other.dy, other.dx)))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for PathInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// now also working when we store pos, straight moves left and direction when
// lookging up the shortest path
fn dijkstra_regular(grid: &Vec<Vec<u8>>) -> u32 {
    // init to max cost
    let mut heat_loss = HashMap::new();
    // queue that always has path with lowest heat_loss and 
    let mut queue = BinaryHeap::new();
    let max_y = (grid.len() - 1) as u32;
    let max_x = (grid[0].len() - 1) as u32;
    let start = (0, 0);
    let goal = (max_y, max_x);
    let start_heat_loss = 0;

    // start
    // to be sure we have the shortest path for a branch we need to
    // use the pos, direction __and__ the straight moves left as keys,
    // since there might be shorter paths for different combinations
    // of these, e.g.
    // pop State { y: 1, x: 5, dy: 0, dx: 1, heat_loss: 20, path: [(0, 2), (1, 2), (1, 5)] }
    // higher loss (0, 5) l23 > 22
    // discards the shortest path, since it has a higher heat loss at that point
    // but it uses a different direction/moves left than the one it's compared
    // against
    heat_loss.insert(((start.0, start.1), (0, 0), 3), start_heat_loss);
    queue.push(
        // start is special, so use 0,0 as dy,dx so we don't eliminate a
        // direction
        // but 3 moves_until_turn since we're not coming from any direction
        PathInfo{y: start.0, x: start.1, dy: 0, dx: 0,
                 moves_until_turn: 3, heat_loss: start_heat_loss, path: vec!()});

    while let Some(path_info) = queue.pop() {
        // println!("Pop {:?}", path_info);
        if (path_info.y, path_info.x) == goal {
            return path_info.heat_loss;
        }

        // we need to check if we already have a path with lower cost
        // at same pos
        // NOTE: also need to check left-over straight moves __and__ direction
        if let Some(cur_min) = heat_loss.get(
                &((path_info.y, path_info.x), 
                  (path_info.dy, path_info.dx),
                  path_info.moves_until_turn)) {
            if path_info.heat_loss > *cur_min {
                // already found better path
                continue;
            }
        }

        // iterate over neighbours filtering out directions we can't choose
        for (ndy, ndx) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
            // can't reverse
            if (ndy, ndx) == (-path_info.dy, -path_info.dx) {
                continue;
            }
            let going_straight = (ndy, ndx) == (path_info.dy, path_info.dx);
            if path_info.moves_until_turn == 0 && going_straight {
                // can't continue in same direction
                continue;
            }

            let new_y = path_info.y as i32 + ndy as i32;
            let new_x = path_info.x as i32 + ndx as i32;
            if new_y < 0 || new_x < 0 || new_y > max_y as i32 || new_x > max_x as i32 {
                // out of bounds
                continue
            }
            let new_y = new_y as u32;
            let new_x = new_x as u32;

            let heat_cost = grid[new_y as usize][new_x as usize];
            let next_heat_loss = path_info.heat_loss + heat_cost as u32;
            let new_moves_until_turn = if going_straight
                { path_info.moves_until_turn - 1 }
                else
                // 2 instead of 3, since we already used up 1 step
                { 2 };
            let cur_min = heat_loss.get(
                &((new_y, new_x), (ndy, ndx), new_moves_until_turn));

            if cur_min == None || next_heat_loss < *cur_min.unwrap() {
                // shorter path than existing
                let mut new_path = path_info.path.clone();
                new_path.push((new_y, new_x));
                // println!("q cost {} mov {} p {:?}", next_heat_loss, new_moves_until_turn, new_path);
                queue.push(
                    PathInfo{
                        y: new_y,
                        x: new_x,
                        dy: ndy, dx: ndx,
                        moves_until_turn: new_moves_until_turn,
                        heat_loss: next_heat_loss,
                        path: new_path});
                // update cost
                heat_loss.insert(
                    ((new_y, new_x), (ndy, ndx), new_moves_until_turn),
                    next_heat_loss);
            } else {
                // println!("skip q y{}x{} {} > {:?}", new_y, new_x, next_heat_loss, cur_min);
            }
        }
    }

    // couldn't find a path
    unreachable!()
}

fn dfs(grid: &Vec<Vec<u8>>) -> u32 {
    let mut stack = Vec::new();
    let max_y = (grid.len() - 1) as u32;
    let max_x = (grid[0].len() - 1) as u32;
    let goal = (max_y, max_x);

    // start
    stack.push(
        // start is special, so use 0,0 as dy,dx so we don't eliminate a
        // direction
        // but 3 moves_until_turn since we're not coming from any direction
        (PathInfo{y: 0, x: 0, dy: 0, dx: 0,
                 moves_until_turn: 3, heat_loss: 0, path: vec!()},
         HashSet::new()));
    let mut min_heat_loss = u32::MAX;
    while let Some((path_info, mut visited)) = stack.pop() {
        if path_info.heat_loss > min_heat_loss {
            continue;
        }
        if (path_info.y, path_info.x) == goal {
            min_heat_loss = std::cmp::min(min_heat_loss, path_info.heat_loss);
            println!("reached exit {}", path_info.heat_loss);
            continue;
        }

        visited.insert((path_info.y, path_info.x));

        // iterate over neighbours filtering out directions we can't choose
        for (ndy, ndx) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
            // can't reverse
            if (ndy, ndx) == (-path_info.dy, -path_info.dx) {
                continue;
            }
            let going_straight = (ndy, ndx) == (path_info.dy, path_info.dx);
            if path_info.moves_until_turn == 0 && going_straight {
                // can't continue in same direction
                continue;
            }

            let new_y = path_info.y as i32 + ndy as i32;
            let new_x = path_info.x as i32 + ndx as i32;
            if new_y < 0 || new_x < 0 || new_y > max_y as i32 || new_x > max_x as i32 {
                // out of bounds
                continue
            }
            let new_y = new_y as u32;
            let new_x = new_x as u32;

            if visited.contains(&(new_y, new_x)) {
                continue;
            }

            let heat_cost = grid[new_y as usize][new_x as usize];
            let next_heat_loss = path_info.heat_loss + heat_cost as u32;
            let new_moves_until_turn = if going_straight
                { path_info.moves_until_turn - 1 }
                else
                // 2 instead of 3, since we already used up 1 step
                { 2 };
            stack.push(
                (PathInfo{
                    y: new_y,
                    x: new_x,
                    dy: ndy, dx: ndx,
                    moves_until_turn: new_moves_until_turn,
                    heat_loss: next_heat_loss,
                    path: vec!()},
                 visited.clone()));
        }
    }
    min_heat_loss
}
