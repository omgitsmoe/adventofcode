use std::io;
use std::fs;
use std::collections::HashMap;

const USE_EXAMPLE: bool = false;

enum Monkey<'a> {
    Static(i64),
    Add((&'a str, &'a str)),
    Sub((&'a str, &'a str)),
    Mul((&'a str, &'a str)),
    Div((&'a str, &'a str)),
    Eq((&'a str, &'a str)),
}

fn evaluate<'a>(monkeys: &HashMap<&'a str, Monkey<'a>>, name: &str) -> i64 {
    if let Some(monkey) = monkeys.get(name) {
        match monkey {
            Monkey::Static(num) => *num,
            Monkey::Add((op1, op2)) => {
                evaluate(monkeys, op1) + evaluate(monkeys, op2)
            },
            Monkey::Sub((op1, op2)) => {
                evaluate(monkeys, op1) - evaluate(monkeys, op2)
            },
            Monkey::Mul((op1, op2)) => {
                evaluate(monkeys, op1) * evaluate(monkeys, op2)
            },
            Monkey::Div((op1, op2)) => {
                evaluate(monkeys, op1) / evaluate(monkeys, op2)
            },
            _ => unreachable!("eq not in pt1!"),
        }
    } else {
        unreachable!("malformed input!")
    }
}

// ugly to do it with a tuple, should use another enum for Ref vs Static number in Monkeys
// and then use that
fn evaluate_pt2<'a>(monkeys: &HashMap<&'a str, Monkey<'a>>, name: &str, ops: &mut Vec<(u8, bool, i64)>, is_humn_branch: &mut bool) -> i64 {
    // is_humn_branch = ...; would be trying to change the reference itself, but want to
    // change the value it points to so use:
    *is_humn_branch = *is_humn_branch | if name == "humn" { true } else { false };
    if let Some(monkey) = monkeys.get(name) {
        match monkey {
            Monkey::Static(num) => *num,
            Monkey::Add((op1, op2)) => {
                let mut op1_has_humn = false;
                let res1 = evaluate_pt2(monkeys, op1, ops, &mut op1_has_humn);
                let mut op2_has_humn = false;
                let res2 = evaluate_pt2(monkeys, op2, ops, &mut op2_has_humn);
                if op1_has_humn {
                    println!("humn + {}", res2);
                    ops.push((b'+', true, res2));
                } else if op2_has_humn {
                    println!("{} + humn", res1);
                    ops.push((b'+', false, res1));
                }
                *is_humn_branch = *is_humn_branch | op1_has_humn | op2_has_humn;
                res1 + res2
            },
            Monkey::Sub((op1, op2)) => {
                let mut op1_has_humn = false;
                let res1 = evaluate_pt2(monkeys, op1, ops, &mut op1_has_humn);
                let mut op2_has_humn = false;
                let res2 = evaluate_pt2(monkeys, op2, ops, &mut op2_has_humn);
                if op1_has_humn {
                    println!("humn - {}", res2);
                    ops.push((b'-', true, res2));
                } else if op2_has_humn {
                    println!("{} - humn", res1);
                    ops.push((b'-', false, res1));
                }
                *is_humn_branch = *is_humn_branch | op1_has_humn | op2_has_humn;
                res1 - res2
            },
            Monkey::Mul((op1, op2)) => {
                let mut op1_has_humn = false;
                let res1 = evaluate_pt2(monkeys, op1, ops, &mut op1_has_humn);
                let mut op2_has_humn = false;
                let res2 = evaluate_pt2(monkeys, op2, ops, &mut op2_has_humn);
                if op1_has_humn {
                    println!("humn * {}", res2);
                    ops.push((b'*', true, res2));
                } else if op2_has_humn {
                    println!("{} * humn", res1);
                    ops.push((b'*', false, res1));
                }
                *is_humn_branch = *is_humn_branch | op1_has_humn | op2_has_humn;
                res1 * res2
            },
            Monkey::Div((op1, op2)) => {
                let mut op1_has_humn = false;
                let res1 = evaluate_pt2(monkeys, op1, ops, &mut op1_has_humn);
                let mut op2_has_humn = false;
                let res2 = evaluate_pt2(monkeys, op2, ops, &mut op2_has_humn);
                if op1_has_humn {
                    println!("humn / {}", res2);
                    ops.push((b'/', true, res2));
                } else if op2_has_humn {
                    println!("{} / humn", res1);
                    ops.push((b'/', false, res1));
                }
                *is_humn_branch = *is_humn_branch | op1_has_humn | op2_has_humn;
                res1 / res2
            },
            Monkey::Eq((op1, op2)) => {
                // 1. which branch op1/2 humn is in
                // 2. which number the other branch evalutates to
                // 3. start with that number and do the reverse operations until we reach
                //    humn
                let mut op1_has_humn = false;
                let res1 = evaluate_pt2(monkeys, op1, ops, &mut op1_has_humn);
                let mut op2_has_humn = false;
                let res2 = evaluate_pt2(monkeys, op2, ops, &mut op2_has_humn);
                println!("root: op1 {} op2 {}", res1, res2);
                if op1_has_humn {
                    println!("humn = {}", res2);
                } else if op2_has_humn {
                    println!("{} = humn", res1);
                }
                if op1_has_humn { res2 } else { res1 }
            },
        }
    } else {
        unreachable!("malformed input!")
    }
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string(if USE_EXAMPLE { "../d21.example.in" } else { "../d21.in" })?;
    let contents = contents.trim();

    let mut monkeys = HashMap::new();
    for line in contents.lines() {
        match line.as_bytes()[line.len() - 1] {
            b'0'..=b'9' => {
                // static monkey
                let (name, number) = line.split_once(":").unwrap();
                let number = number.trim().parse::<i64>().expect("Should not fail!");
                monkeys.insert(name, Monkey::Static(number));
            },
            _ => {
                let mut iter = line.split_whitespace();
                let mut name = iter.next().unwrap();
                name = &name[0..name.len() - 1];

                let operand1 = iter.next().unwrap();
                let operator = iter.next().unwrap();
                let operand2 = iter.next().unwrap();

                let monkey = match operator.as_bytes()[0] {
                    b'+' => Monkey::Add((operand1, operand2)),
                    b'-' => Monkey::Sub((operand1, operand2)),
                    b'*' => Monkey::Mul((operand1, operand2)),
                    b'/' => Monkey::Div((operand1, operand2)),
                    _ => unreachable!("panic"),
                };

                monkeys.insert(name, monkey);
            },
        }
    }

    println!("Part1: Root monkey is yelling: {}", evaluate(&monkeys, "root"));

    // part2
    let (root_op1, root_op2) = match monkeys.get("root").unwrap() {
        Monkey::Add((operand1, operand2)) |
        Monkey::Sub((operand1, operand2)) |
        Monkey::Mul((operand1, operand2)) |
        Monkey::Div((operand1, operand2)) => (*operand1, *operand2),
        _ => unreachable!("oopsie"),
    };
    monkeys.insert("root", Monkey::Eq((root_op1, root_op2)));
    let mut b = false;
    let mut ops = vec!{};
    let other_branch_num = evaluate_pt2(&monkeys, "root", &mut ops, &mut b);
    // do the reverse operations (from top to bottom) using the number of the other branch
    // -> at the end we will get the number neeeded for "humn"
    let mut num_needed = other_branch_num;
    for (u8_op, humn_is_lhs, other_num) in ops.iter().rev() {
        // println!("op {} lhs {} other {}", *u8_op as char, humn_is_lhs, other_num);
        match u8_op {
            // inverse operations:
            b'+' => {
                num_needed -= other_num;
            },
            b'*' => {
                num_needed /= other_num;
            },
            b'-' => {
                if *humn_is_lhs {
                    // hmn - 7 -> hmn + 7
                    num_needed = num_needed + other_num;
                } else {
                    // 7 - hmn -> 7 - hmn
                    num_needed = other_num - num_needed;
                }
            },
            b'/' => {
                if *humn_is_lhs {
                    // hmn / 2 -> hmn * 2
                    num_needed *= other_num;
                } else {
                    // 2 / hmn -> hmn / 2
                    num_needed = num_needed / other_num;
                }
            },
            _ => unreachable!("ooopsie u8op"),

        }
    }

    monkeys.insert("humn", Monkey::Static(num_needed));

    println!("Part2: I should yell {} -> {} = {}", num_needed,
             evaluate(&monkeys, root_op1), evaluate(&monkeys, root_op2));

    Ok(())
}
