use std::io;
use std::fs;
use std::cmp;
use std::collections::HashMap;
use std::time::{Instant};

const USE_EXAMPLE: bool = false;

const ROBOTS_ORE: u8 = 1 << 0;
const ROBOTS_CLAY: u8 = 1 << 1;
const ROBOTS_OBS: u8 = 1 << 2;
const ROBOTS_GEO: u8 = 1 << 3;
const ROBOTS_ALL: u8 = ROBOTS_ORE | ROBOTS_CLAY | ROBOTS_OBS | ROBOTS_GEO;

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
    // credit: KeeperOfTheFeels
    // if we skip buying a robot even though we could buy it
    // -> makes no sense (since it could already be mining) to buy it next turn so prune that option
    // using this pt1 went from ~6-10s to ~230ms
    available_robots: u8,

    ore_robot_num: u8,
    clay_robot_num: u8,
    obsidian_robot_num: u8,
    geode_robot_num: u8,

    ore: u8,
    clay: u8,
    obsidian: u8,
}

impl State {
    fn new(time_left: u8) -> Self {
        State {
            time_left: time_left,
            available_robots: ROBOTS_ALL,
            
            ore_robot_num: 1,
            clay_robot_num: 0,
            obsidian_robot_num: 0,
            geode_robot_num: 0,

            ore: 0,
            clay: 0,
            obsidian: 0,
        }
    }

    fn can_pay(&self, cost: Cost) -> bool {
        if self.ore >= cost.ore &&
                self.clay >= cost.clay &&
                self.obsidian >= cost.obsidian {
            true
        } else {
            false
        }
    }

    fn mine(mut self) -> Self {
        self.time_left -= 1;
        self.ore += self.ore_robot_num;
        self.clay += self.clay_robot_num;
        self.obsidian += self.obsidian_robot_num;
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
        self.ore -= cost.ore;
        self.clay -= cost.clay;
        self.obsidian -= cost.obsidian;
        // bought a robot -> allowed to buy all robots again
        self.available_robots = ROBOTS_ALL;

        self
    }

    fn set_available(&mut self, available_robots: u8) -> &mut Self {
        self.available_robots = available_robots;
        self
    }

    fn cap_resources(self, max_needed: Cost) -> Self {
        let mut capped = self;
        capped.ore = cmp::min(capped.ore, max_needed.ore * capped.time_left);
        capped.clay = cmp::min(capped.clay, max_needed.clay * capped.time_left);
        capped.obsidian = cmp::min(capped.clay, max_needed.obsidian * capped.time_left);
        self
    }
}

enum Robot {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

// cache tracks maximum relative geodes that can be mined from that state
fn dfs(bp: &Blueprint, state: State, cache: &mut HashMap<State, u16>) -> u16 {
    if let Some(cached_max) = cache.get(&state.cap_resources(bp.max_needed)) {
    // if let Some(cached_max) = cache.get(&state) {
        return *cached_max;
    }

    if state.time_left == 0 {
        return 0u16;
    }

    let mut max = 0u16;
    let mut next_available_robots = 0;

    // buy a geode_robot if we can, since we want to maximize geodes
    // NOTE: greedy, but might be incorrect in some cases
    // alternative would be to remove to de-indent the else branche, remove setting ROBOTS_GEO
    // flag and use the below instead:
    //
    // if !state.can_pay(bp.geode_robot) {
    //     next_available_robots |= ROBOTS_GEO;
    // } else if (state.available_robots & ROBOTS_GEO) != 0 {
    //     max = cmp::max(
    //         max,
    //         // add geodes the robot we just bought will mine with the time left
    //         // to max of relative geodes mined we can reach from that state
    //         dfs(bp, state.mine().pay(&bp, Robot::Geode), cache) + (state.time_left - 1) as u16
    //     );
    // }
    // => only ~1s more needed for part2

    if state.can_pay(bp.geode_robot) {
        max = cmp::max(
            max,
            // add geodes the robot we just bought will mine with the time left
            // to max of relative geodes mined we can reach from that state
            dfs(bp, state.mine().pay(&bp, Robot::Geode), cache) + (state.time_left - 1) as u16
        );
    } else {
        // only consider other options if we can't
        // not enough resources to buyt the geode_robot -> we can try to buy it next term
        next_available_robots |= ROBOTS_GEO;

        
        // max amount of geodes we can reach from this point, assuming we buy one
        // robot every minute
        // (== sum from 0..time_left)
        // = ((n-1)*n)/2
        let max_potential_geodes = ((state.time_left as u16 - 1) * state.time_left as u16) / 2;
        // makes no sense to continue down this branch if our max potential is already less
        // than our max we have found so far
        // credit: Boojum and KeeperOfTheFeels
        if max_potential_geodes > max {
            let enough_ore_robots = state.ore_robot_num >= bp.max_needed.ore;
            let enough_clay_robots = state.clay_robot_num >= bp.max_needed.clay;
            let enough_obsidian_robots = state.obsidian_robot_num >= bp.max_needed.obsidian;

            // try buying each robot, that produces materials for the geode_robot and recurse
            if !state.can_pay(bp.ore_robot) {
                next_available_robots |= ROBOTS_ORE;
            // NOTE: only get another ore robot if we can buy less than one ore robot per minute
            // (would not make sense to get more (other than geode robots) since we can only buy
            //  one robot per minute)
            } else if !enough_ore_robots && (state.available_robots & ROBOTS_ORE) != 0 {
                max = cmp::max(
                    max,
                    dfs(bp, state.mine().pay(&bp, Robot::Ore), cache)
                );
            }
            // only get another clay robot if we can buy less than one clay robot per minute
            if !state.can_pay(bp.clay_robot) {
                next_available_robots |= ROBOTS_CLAY;
            } else if !enough_clay_robots && (state.available_robots & ROBOTS_CLAY) != 0 {
                max = cmp::max(
                    max,
                    dfs(bp, state.mine().pay(&bp, Robot::Clay), cache)
                );
            }
            if !state.can_pay(bp.obsidian_robot) {
                next_available_robots |= ROBOTS_OBS;
            } else if !enough_obsidian_robots && (state.available_robots & ROBOTS_OBS) != 0 {
                max = cmp::max(
                    max,
                    dfs(bp, state.mine().pay(&bp, Robot::Obsidian), cache)
                );
            }

            // NOTE: checking for next_available_robots being 0 only needed if we don't try to
            // greedily buy geode_robots:
            if next_available_robots != 0 {
                // recurse without buying
                max = cmp::max(
                    max,
                    dfs(bp, *state.mine().set_available(next_available_robots), cache)
                );
            }
        }
    }

    // NOTE: could further reduce the resources to the max_needed so we get more cache hits
    // cmp::min(state.ore, bp.max_needed.ore), ...
    // credit: KeeperOfTheFeels
    cache.insert(state.cap_resources(bp.max_needed), max);
    // -> no real difference performance-wise
    // cache.insert(state, max);

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

    let timer = Instant::now();
    let mut qualities = Vec::new();
    for bp in &blueprints {
        let mut cache = HashMap::new();

        // very similar to d16, so use a recursive dfs again
        let max_geodes = dfs(&bp, State::new(24), &mut cache);
        // TODO could you re-use the t=24 state as starting point for pt2 with t=32?
        qualities.push(max_geodes * bp.id as u16);
    }

    println!("Part1: Sum of blueprint qualities: {}", qualities.iter().sum::<u16>());
    println!("Part1 took {:?}", timer.elapsed());

    let timer = Instant::now();

    let mut max_geodes_product = 1;
    for i in 0..cmp::min(3, blueprints.len()) {
        let mut cache = HashMap::new();

        let max_geodes = dfs(&blueprints[i], State::new(32), &mut cache);
        println!("Max geodes for t=32 and bp={}: {}", i+1, max_geodes);
        max_geodes_product *= max_geodes;
    }

    println!("Part2: Product of max geodes of the first three blueprints: {}", max_geodes_product);
    println!("Part2 took {:?}", timer.elapsed());
    Ok(())
}
