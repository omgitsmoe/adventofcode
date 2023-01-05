use std::io;
use std::fs;
use std::cmp;
use std::ops;
use std::str::FromStr;
use std::collections::HashSet;

const USE_EXAMPLE: bool = false;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

// implement the FromStr trait for our custom Point struct so we can use "".parse()
// for parsing a str into a Point
impl FromStr for Point {
    // no proper err type
    type Err = bool;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(",").map(|num_str| num_str.parse::<i32>().expect("Should not fail!"));
        let ret = Point{
            x: iter.next().unwrap(),
            y: iter.next().unwrap(),
            z: iter.next().unwrap(),
        };
        // println!("{:?}", ret);
        Ok(ret)
    }
}

impl ops::Add<&Point> for &Point {
    type Output = Point;
    fn add(self, rhs: &Point) -> Self::Output {
        Point { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string(if USE_EXAMPLE { "../d18.example.in" } else { "../d18.in" })?;
    let contents = contents.trim();

    let points = contents
        .lines()
        .map(|line| line.parse::<Point>().expect("Should not  fail!"))
        .collect::<HashSet<_>>();

    let neighbours = [
        // above
        Point{ x: 0, y: 1, z: 0 },
        // below
        Point{ x: 0, y: - 1, z: 0 },
        // front
        Point{ x: 0, y: 0, z: 1 },
        // back
        Point{ x: 0, y: 0, z: - 1 },
        // left
        Point{ x: - 1, y: 0, z: 0 },
        // right
        Point{ x: 1, y: 0, z: 0 }
    ];
    let mut air = HashSet::new();
    let mut sides_total: u64 = 0;
    let mut max = Point{ x: 0, y: 0, z: 0 };
    let mut min = Point{ x: i32::MAX, y: i32::MAX, z: i32::MAX };
    for p in points.iter() {
        // sum can only sum to the same type -> have to use u8, but no risk of overflow
        // (annoying for other things though)
        // let sides_wo_neighbour: u8 = [
        //     // above
        //     !points.contains(&point{ x: p.x, y: p.y + 1, z: p.z }) as u8,
        //     // below
        //     !points.contains(&point{ x: p.x, y: p.y - 1, z: p.z }) as u8,
        //     // front
        //     !points.contains(&point{ x: p.x, y: p.y, z: p.z + 1 }) as u8,
        //     // back
        //     !points.contains(&point{ x: p.x, y: p.y, z: p.z - 1 }) as u8,
        //     // left
        //     !points.contains(&point{ x: p.x - 1, y: p.y, z: p.z }) as u8,
        //     // right
        //     !points.contains(&point{ x: p.x + 1, y: p.y, z: p.z }) as u8].into_iter().sum();
        // sides_total += sides_wo_neighbour as u64;

        neighbours
            .iter()
            .map(|n| p + n)
            .for_each(|pp| {
                if !points.contains(&pp) {
                    // air
                    sides_total += 1;
                    air.insert(pp);
                }
            });


        // find AABB for pt2
        max.x = cmp::max(max.x, p.x);
        max.y = cmp::max(max.y, p.y);
        max.z = cmp::max(max.z, p.z);
        min.x = cmp::min(min.x, p.x);
        min.y = cmp::min(min.y, p.y);
        min.z = cmp::min(min.z, p.z);
    }

    println!("Part1: Surface area of {} sides", sides_total);

    // start from known exterior air point (here next to min)
    // discover all connected air -> exterior air
    let mut exterior_air = HashSet::new();
    let mut q = vec!(&min + &Point{ x: -1, y: -1, z: -1 });
    // use one beyond min/max point coords so we can start outside the bounds
    max = &max + &Point{ x: 1, y: 1, z: 1 };
    min = &min + &Point{ x: -1, y: -1, z: -1 };

    while q.len() > 0 {
        let p = q.pop().unwrap();
        exterior_air.insert(p.clone());

        for n in &neighbours {
            let next = &p + n;
            if next.x > max.x || next.y > max.y || next.z > max.z ||
                next.x < min.x || next.y < min.y || next.z < min.z {
                // out of bounds
                continue;
            } else if exterior_air.contains(&next) || points.contains(&next) {
                // already visited or cube
                continue;
            }
            q.push(next);
        }
    }

    let mut exterior_sides_total: u64 = 0;
    for p in points.iter() {
        let exterior_sides = neighbours
            .iter()
            .map(|n| p + n)
            .filter(|p| !points.contains(p) && exterior_air.contains(p))
            .count();

        exterior_sides_total += exterior_sides as u64;
    }

    println!("Part2: Surface area of exterior {} sides", exterior_sides_total);

    Ok(())
}
