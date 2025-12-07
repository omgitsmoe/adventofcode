use std::fs;
// NOTE: constraint: use rust as a purely functional language, so no mutation, no for loops
//       no side effects other than reading in the file as first step
// -> not really idiomatic, turned out to be more cumbersome than I thought
//    matching on vecs instead lists, if you want to avoid copies you run into ownership
//    problems quickly and even then it seems very hard or even impossible to avoid copies

// cargo run --bin d06
fn main() {
    let input = fs::read_to_string("../d06.in").unwrap();
    let input2 = input.clone();
    let rows_bare = input2
        .split('\n')
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let rows = input
        .split('\n')
        .map(|l| l.split_whitespace().collect::<Vec<_>>())
        .filter(|r| !r.is_empty())
        .collect::<Vec<_>>();
    let problems = build_problems(rows);
    let part1 =
        problems
        .iter().fold(0, |sum, p| {
            sum + p.solve()
        });

    println!("Part1: {}", part1);

    let swapped =
        (0..rows_bare[0].len()).map(|col_idx|
            rows_bare
            .iter()
            .map(|row_as_column| row_as_column.as_bytes()[col_idx] as char)
            .collect::<String>()
        ).collect::<Vec<String>>();
    let problems =
        swapped
        .iter()
        .fold(vec![Problem{ numbers: vec![], operator: None }], |mut acc, col_as_row| {
            if col_as_row.trim().is_empty() {
                return [acc, vec![Problem{ numbers: vec![], operator: None }]].concat();
            }

            let (current, rest) = acc.split_last_mut().unwrap();
            let numbers = std::mem::take(&mut current.numbers);
            let (op, col_as_row) = if col_as_row.ends_with('*') {
                (Some(Operator::Mul), col_as_row[..col_as_row.len() - 1].trim())
            } else if col_as_row.ends_with('+') {
                (Some(Operator::Add), col_as_row[..col_as_row.len() - 1].trim())
            } else {
                (current.operator, col_as_row.trim())
            };
            let current = {
                let num = col_as_row.parse::<i64>().unwrap();
                Problem{numbers: [numbers, vec![num]].concat(), operator: op}
            };

            [rest, &mut [current]].concat()
        });
    let part2 =
        problems
        .iter().fold(0, |sum, p| {
            sum + p.solve()
        });

    println!("Part2: {}", part2);
}

fn build_problems(rows: Vec<Vec<&str>>) -> Vec<Problem> {
    let num_problems = rows[0].len();
    let initial = std::iter::repeat_n(
        Problem {
            numbers: vec![],
            operator: None,
        },
        num_problems,
    )
    .collect::<Vec<_>>();
    rows.iter().fold(initial, |acc, row| {
        let (todos, done) = row
            .iter()
            .fold((acc, vec![]), |(mut todos, done), num_or_op_str| {
                let (head, tail) = todos.split_first_mut().unwrap();

                // so we can move the numbers
                let numbers = std::mem::take(&mut head.numbers);

                let head = match num_or_op_str {
                    &"*" => Problem {
                        operator: Some(Operator::Mul),
                        numbers,
                    },
                    &"+" => Problem {
                        operator: Some(Operator::Add),
                        numbers,
                    },
                    str => {
                        let num = str.parse::<i64>().unwrap();
                        Problem {
                            numbers: [numbers, vec![num]].concat(),
                            ..*head
                        }
                    }
                };

                (tail.to_vec(), [done, vec![head]].concat())
            });

        assert!(todos.is_empty(), "todos must be empty!");
        done
    })
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Operator {
    Add,
    Mul,
}

#[derive(Clone, Debug)]
struct Problem {
    pub numbers: Vec<i64>,
    pub operator: Option<Operator>,
}

impl Problem {
    fn solve(&self) -> i64 {
        let op = match self.operator {
            Some(Operator::Add) => |a, b| a + b,
            Some(Operator::Mul) => |a, b| a * b,
            None => unreachable!(),
        };

        let initial = if self.operator == Some(Operator::Add) { 0 } else { 1 };
        self.numbers.iter().fold(initial, op)
    }
}
