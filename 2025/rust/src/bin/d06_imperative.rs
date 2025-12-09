use std::fs;
use std::time::Instant;

fn main() {
    let start = Instant::now();
    let input = fs::read_to_string("../d06.in").expect("read input");
    let t0 = Instant::now();

    // -------------------------
    // Part 1: build from tokens
    // -------------------------
    // Each non-empty input line contains whitespace-separated tokens.
    // We build one Problem per column (i.e., per token index).
    let token_rows: Vec<Vec<&str>> = input
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| l.split_whitespace().collect())
        .collect();

    let part1 = if token_rows.is_empty() {
        0i64
    } else {
        solve_part1_from_token_rows(&token_rows)
    };

    println!("Part1: {}", part1);
    let t1 = Instant::now();

    // ------------------------------------------
    // Part 2: interpret columns as vertical data
    // ------------------------------------------
    // We must preserve trailing spaces: treat each line as bytes,
    // determine the max width and read missing bytes as ' '.
    let byte_lines: Vec<&[u8]> = input
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| l.as_bytes())
        .collect();

    let part2 = if byte_lines.is_empty() {
        0i64
    } else {
        solve_part2_from_byte_columns(&byte_lines)
    };

    let t2 = Instant::now();

    println!("Part2: {}", part2);
    eprintln!(
        "timings: total={:.3}ms build1={:.3}ms build2={:.3}ms",
        (t2 - start).as_secs_f64() * 1000.0,
        (t1 - t0).as_secs_f64() * 1000.0,
        (t2 - t1).as_secs_f64() * 1000.0
    );
}

/// Operator for a Problem: Add or Mul
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operator {
    Add,
    Mul,
}

/// Problem: a sequence of numbers and an operator (Add or Mul).
#[derive(Debug)]
struct Problem {
    numbers: Vec<i64>,
    operator: Option<Operator>,
}

impl Problem {
    /// Solve the problem: reduce numbers by operator.
    /// Expects operator to be Some(Add|Mul).
    #[inline(always)]
    fn solve(&self) -> i64 {
        match self.operator {
            Some(Operator::Add) => self.numbers.iter().fold(0i64, |acc, &n| acc + n),
            Some(Operator::Mul) => self.numbers.iter().fold(1i64, |acc, &n| acc * n),
            None => panic!("Problem with no operator: {:?}", self),
        }
    }
}

/// Build problems from tokenized rows (whitespace tokens), then solve sum of each problem.
/// This is the fast, idiomatic imperative version for Part 1.
fn solve_part1_from_token_rows(rows: &[Vec<&str>]) -> i64 {
    let n_rows = rows.len();
    if n_rows == 0 {
        return 0;
    }
    let width = rows[0].len();

    // Pre-allocate problems (one per column).
    let mut problems: Vec<Problem> = Vec::with_capacity(width);
    for _ in 0..width {
        problems.push(Problem {
            numbers: Vec::with_capacity(n_rows / 2 + 2),
            operator: None,
        });
    }

    // Fill problems column-wise. This uses direct indexing and avoids reallocation.
    for row in rows {
        for (col, token) in row.iter().enumerate() {
            match *token {
                "+" => problems[col].operator = Some(Operator::Add),
                "*" => problems[col].operator = Some(Operator::Mul),
                s => {
                    // parse integer; unwrap is fine for contest-style input
                    let v: i64 = s.parse().expect("expected integer token");
                    problems[col].numbers.push(v);
                }
            }
        }
    }

    // Sum up solves
    let mut sum = 0i64;
    for p in &problems {
        sum += p.solve();
    }
    sum
}

/// Parse columns from raw byte lines and build problems for Part 2.
/// Very hot inner loops, minimal allocations: we mutate a current Problem in-place,
/// pushing completed problems to the result vector on blank columns.
/// This is optimized and idiomatic Rust (no clones).
fn solve_part2_from_byte_columns(lines: &[&[u8]]) -> i64 {
    let n_lines = lines.len();
    if n_lines == 0 {
        return 0;
    }

    // Determine max width (respect trailing spaces)
    let width = lines.iter().map(|l| l.len()).max().unwrap_or(0);

    // Result list of problems
    let mut problems: Vec<Problem> = Vec::with_capacity(width / 4 + 4);

    // Current problem being built
    let mut current = Problem {
        numbers: Vec::with_capacity(n_lines / 2 + 2),
        operator: None,
    };

    // Buffer to parse digits for a column (we accumulate a number while scanning rows)
    // We'll reuse a primitive i64 accumulator per column.
    for col in 0..width {
        let mut saw_any = false;
        let mut num_acc: i64 = 0;
        let mut have_num = false;
        let mut op_in_col: Option<Operator> = None;

        // iterate over rows top-to-bottom, get byte or ' ' if out-of-range
        for r in 0..n_lines {
            let b = if col < lines[r].len() {
                lines[r][col]
            } else {
                b' '
            };
            match b {
                b'0'..=b'9' => {
                    saw_any = true;
                    have_num = true;
                    // safe: numbers are modest per puzzle
                    num_acc = num_acc * 10 + (b - b'0') as i64;
                }
                b'+' => {
                    saw_any = true;
                    // if a number was in progress, commit it before operator
                    if have_num {
                        current.numbers.push(num_acc);
                        num_acc = 0;
                        have_num = false;
                    }
                    op_in_col = Some(Operator::Add);
                }
                b'*' => {
                    saw_any = true;
                    if have_num {
                        current.numbers.push(num_acc);
                        num_acc = 0;
                        have_num = false;
                    }
                    op_in_col = Some(Operator::Mul);
                }
                _ => {
                    // whitespace — nothing to do
                }
            }
        }

        // if we finished a number at end of column, commit it
        if have_num {
            current.numbers.push(num_acc);
        }

        if !saw_any {
            // blank column -> finalize current problem (if it has any numbers or an operator).
            // If current problem is empty (no numbers and no operator) we skip pushing.
            if !current.numbers.is_empty() || current.operator.is_some() {
                // If operator is still None here, it's likely invalid input; but we push anyway.
                problems.push(current);
            }
            // start a new current problem
            current = Problem {
                numbers: Vec::with_capacity(n_lines / 2 + 2),
                operator: None,
            };
        } else {
            // Update current operator if we saw one in this column
            if let Some(op) = op_in_col {
                current.operator = Some(op);
            }
        }
    }

    // After all columns processed, push the last problem if non-empty
    if !current.numbers.is_empty() || current.operator.is_some() {
        problems.push(current);
    }

    // Solve all problems and sum
    let mut total = 0i64;
    for p in &problems {
        total += p.solve();
    }
    total
}

