use std::fs;

fn main() {
    let input = fs::read_to_string("../d06.in").unwrap();

    // -------- Part 1 --------

    let rows: Vec<Vec<&str>> = input
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| l.split_whitespace().collect())
        .collect();

    let problems = build_problems_functional(&rows);

    let part1: i64 = problems.iter().map(Problem::solve).sum();
    println!("Part1: {}", part1);

    // -------- Part 2 (column interpretation) --------

    let transposed: Vec<String> = transpose_lines(&input);

    let problems2 = build_problems_from_strings(&transposed);

    let part2: i64 = problems2.iter().map(Problem::solve).sum();
    println!("Part2: {}", part2);
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Operator {
    Add,
    Mul,
}

#[derive(Clone, Debug)]
struct Problem {
    numbers: Vec<i64>,
    operator: Option<Operator>,
}

impl Problem {
    fn solve(&self) -> i64 {
        let op = match self.operator {
            Some(Operator::Add) => |a, b| a + b,
            Some(Operator::Mul) => |a, b| a * b,
            None => unreachable!(),
        };

        let init = if self.operator == Some(Operator::Add) { 0 } else { 1 };
        self.numbers.iter().fold(init, op)
    }
}

//
// ---------- Part1 builder (row-wise) ----------
//
fn build_problems_functional(rows: &[Vec<&str>]) -> Vec<Problem> {
    // One Problem per column
    let width = rows.first().map(|r| r.len()).unwrap_or(0);

    (0..width)
        .map(|col| {
            // collect all tokens in this column
            let tokens = rows.iter().map(|row| row[col]);

            tokens.fold(
                Problem { numbers: vec![], operator: None },
                |acc, tok| match tok {
                    "*" => Problem { operator: Some(Operator::Mul), ..acc },
                    "+" => Problem { operator: Some(Operator::Add), ..acc },
                    n => {
                        let mut nums = acc.numbers.clone();
                        nums.push(n.parse().unwrap());
                        Problem { numbers: nums, ..acc }
                    }
                },
            )
        })
        .collect()
}

//
// ---------- Part2 helpers (column interpretation) ----------
//

// Transpose grid of characters (no mutation, no indices inside loops)
fn transpose_lines(input: &str) -> Vec<String> {
    let lines: Vec<&str> = input.lines().filter(|l| !l.is_empty()).collect();
    let width = lines.first().map(|l| l.len()).unwrap_or(0);

    (0..width)
        .map(|i| lines.iter().map(|line| line.as_bytes()[i] as char).collect())
        .collect()
}

fn build_problems_from_strings(columns: &[String]) -> Vec<Problem> {
    columns.iter().fold(
        vec![Problem {
            numbers: vec![],
            operator: None,
        }],
        |acc, col| {
            let trimmed = col.trim_end();

            // blank column means: start next problem block
            if trimmed.is_empty() {
                let mut out = acc.clone();
                out.push(Problem {
                    numbers: vec![],
                    operator: None,
                });
                return out;
            }

            // otherwise parse the characters
            let mut out = acc.clone();
            let last = out.last_mut().unwrap().clone();

            let parsed = parse_column(col);

            let updated = Problem {
                numbers: [last.numbers, parsed.numbers].concat(),
                operator: parsed.operator.or(last.operator),
            };

            *out.last_mut().unwrap() = updated;
            out
        },
    )
}

fn parse_column(col: &str) -> Problem {
    col.chars().fold(
        Problem {
            numbers: vec![],
            operator: None,
        },
        |acc, c| match c {
            '0'..='9' => {
                // append digit to last number or start new
                let mut nums = acc.numbers.clone();
                if let Some(last) = nums.last_mut() {
                    *last = *last * 10 + (c as i64 - '0' as i64);
                } else {
                    nums.push((c as i64) - ('0' as i64));
                }
                Problem {
                    numbers: nums,
                    ..acc
                }
            }
            '+' => Problem {
                operator: Some(Operator::Add),
                ..acc
            },
            '*' => Problem {
                operator: Some(Operator::Mul),
                ..acc
            },
            _ => acc, // whitespace ignored
        },
    )
}

