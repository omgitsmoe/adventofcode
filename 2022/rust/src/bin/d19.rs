use std::io;
use std::fs;
use std::cmp;
use std::collections::HashMap;

const USE_EXAMPLE: bool = false;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Cost {
    ore: u8,
    clay: u8,
    obsidian: u8,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Blueprint {
    id: u8,
    ore_robot: Cost,
    clay_robot: Cost,
    obsidian_robot: Cost,
    geode_robot: Cost,

    // max of any ore that's needed for buying a robot
    max_needed: Cost,
}

// derive Copy -> type is not move by default anymore, but copy by default
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct State {
    time_left: u8,

    ore_robot_num: u8,
    clay_robot_num: u8,
    obsidian_robot_num: u8,
    geode_robot_num: u8,

    ore: u16,
    clay: u16,
    obsidian: u16,
    geodes: u16,
}

impl State {
    fn new(time_left: u8) -> Self {
        State {
            time_left: time_left,
            
            ore_robot_num: 1,
            clay_robot_num: 0,
            obsidian_robot_num: 0,
            geode_robot_num: 0,

            ore: 0,
            clay: 0,
            obsidian: 0,
            geodes: 0,
        }
    }

    fn can_pay(&self, cost: Cost) -> bool {
        if self.ore >= cost.ore as u16 &&
                self.clay >= cost.clay as u16 &&
                self.obsidian >= cost.obsidian as u16 {
            true
        } else {
            false
        }
    }

    fn mine(mut self) -> Self {
        self.time_left -= 1;
        self.ore += self.ore_robot_num as u16;
        self.clay += self.clay_robot_num as u16;
        self.obsidian += self.obsidian_robot_num as u16;
        self.geodes += self.geode_robot_num as u16;
        self
    }

    fn pay(mut self, bp: &Blueprint, robot: Robot) -> Self {
        // NOTE: assumes self can_pay for the robot
        let cost;
        match robot {
            Robot::Ore => {
                cost = bp.ore_robot;
                self.ore_robot_num += 1;
            },
            Robot::Clay => {
                cost = bp.clay_robot;
                self.clay_robot_num += 1;
            },
            Robot::Obsidian => {
                cost = bp.obsidian_robot;
                self.obsidian_robot_num += 1;
            },
            Robot::Geode => {
                cost = bp.geode_robot;
                self.geode_robot_num += 1;
            },
        }
        self.ore -= cost.ore as u16;
        self.clay -= cost.clay as u16;
        self.obsidian -= cost.obsidian as u16;

        self
    }
}

enum Robot {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

fn dfs(bp: &Blueprint, state: State, cache: &mut HashMap<State, u16>) -> u16 {
    if let Some(cached_max) = cache.get(&state) {
        return *cached_max;
    }

    if state.time_left == 0 {
        return state.geodes as u16;
    }

    let mut max = 0u16;

    // buy a geode_robot if we can, since we want to maximize geodes
    if state.can_pay(bp.geode_robot) {
        max = cmp::max(
            max,
            dfs(bp, state.mine().pay(&bp, Robot::Geode), cache)
        );
    } else {
        // only consider other options if we can't
        
        // recurse without buying
        max = cmp::max(
            max,
            dfs(bp, state.mine(), cache)
        );

        // try buying each robot, that produces materials for the geode_robot and recurse
        // NOTE: only get another ore robot if we can buy less than one ore robot per minute
        // (would not make sense to get more (other than geode robots) since we can only buy
        //  one robot per minute)
        if state.ore_robot_num < bp.max_needed.ore && state.can_pay(bp.ore_robot) {
            max = cmp::max(
                max,
                dfs(bp, state.mine().pay(&bp, Robot::Ore), cache)
            );
        }
        // only get another clay robot if we can buy less than one clay robot per minute
        if state.clay_robot_num < bp.max_needed.clay && state.can_pay(bp.clay_robot) {
            max = cmp::max(
                max,
                dfs(bp, state.mine().pay(&bp, Robot::Clay), cache)
            );
        }
        if state.obsidian_robot_num < bp.max_needed.obsidian && state.can_pay(bp.obsidian_robot) {
            max = cmp::max(
                max,
                dfs(bp, state.mine().pay(&bp, Robot::Obsidian), cache)
            );
        }
    }

    cache.insert(state, max);

    max
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string(if USE_EXAMPLE { "../d19.example.in" } else { "../d19.in" })?;
    let contents = contents.trim();
    let mut iter = contents
        .lines()
        .flat_map(|l| l.split_whitespace()
            // check last char being numeric since Blueprint 1: would match otherwise
            // (can discard since bps are sequential)
            .flat_map(|word| if word.chars().rev().next().unwrap().is_numeric()
                     // .ok() would just discard err and convert to Option
                     { Some(word.parse::<u8>().expect("should not fail")) } else { None }));
    let mut blueprints = Vec::new();
    let mut id = 1u8;
    while let Some(ore_robot_cost) = iter.next() {
        let mut bp = Blueprint {
                id,
                ore_robot: Cost { ore: ore_robot_cost, clay: 0, obsidian: 0 },
                clay_robot: Cost { ore: iter.next().unwrap(), clay: 0, obsidian: 0 },
                obsidian_robot: Cost { ore: iter.next().unwrap(), clay: iter.next().unwrap(), obsidian: 0 },
                geode_robot: Cost { ore: iter.next().unwrap(), clay: 0, obsidian: iter.next().unwrap() },
                max_needed: Cost{ ore: 0, clay: 0, obsidian: 0 },
        };
        bp.max_needed.ore =
            cmp::max(bp.ore_robot.ore,
                     cmp::max(bp.clay_robot.ore,
                              cmp::max(bp.obsidian_robot.ore, bp.geode_robot.ore)));
        bp.max_needed.clay = bp.obsidian_robot.clay;
        bp.max_needed.obsidian = bp.geode_robot.obsidian;
        
        blueprints.push(bp);

        id += 1;
    }

    let mut qualities = Vec::new();
    for bp in &blueprints {
        let mut cache = HashMap::new();

        // very similar to d16, so use a recursive dfs again
        let max_geodes = dfs(&bp, State::new(24), &mut cache);
        // TODO could you re-use the t=24 state as starting point for pt2 with t=32?
        qualities.push(max_geodes * bp.id as u16);
    }

    println!("Part1: Sum of blueprint qualities: {}", qualities.iter().sum::<u16>());

    let mut max_geodes_product = 1;
    for i in 0..cmp::min(3, blueprints.len()) {
        let mut cache = HashMap::new();

        let max_geodes = dfs(&blueprints[i], State::new(32), &mut cache);
        println!("Max geodes for t=32 and bp={}: {}", i+1, max_geodes);
        max_geodes_product *= max_geodes;
    }

    println!("Part2: Product of max geodes of the first three blueprints: {}", max_geodes_product);
    Ok(())
}
