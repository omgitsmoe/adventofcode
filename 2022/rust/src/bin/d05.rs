use std::io;
use std::fs;

#[derive(Debug)]
struct Move {
    num: u8,
    src: u8,
    dest: u8,
}

fn print_top_crates(stacks: &Vec<Vec<u8>>) {
    print!("Top stacks are '");
    for stack in stacks {
        print!("{}", *stack.last().unwrap() as char);
    }
    println!("'");
}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d05.in")?;

    let mut stacks: Vec<Vec<u8>> = Vec::new();
    let mut lines_iter = contents.lines();
    // by_ref (doesn't consume it) so we can continue using it later
    for ln in lines_iter.by_ref().take_while(|ln| !ln.is_empty()) {
        if ln.starts_with(" 1") {
            // reverse vecs
            // stacks = stacks.into_iter().map(|s| s.into_iter().rev().collect()).collect();
            for stack in &mut stacks {
                stack.reverse();
            }
            break;
        }
        let mut iter = ln.bytes().peekable();
        let mut col = 0;
        loop {
            match iter.peek() {
                Some(b'[') => {
                    while stacks.len() <= col {
                        stacks.push(Vec::new());
                    }
                    // skip returns a Skip struct and changes the iterator type
                    // (taking ownership)
                    // while not actually consuming the items
                    // -> use itertools::dropping or .nth (0 to skip 1, 1 to skip 2 items...)
                    // iter.skip(1);
                    // skip 1
                    iter.nth(0);
                    stacks[col].push(iter.next().unwrap());
                    // skip 2
                    iter.nth(1);
                },
                None => {
                    break;
                },
                _ => {
                    // skip 4
                    iter.nth(3);
                },
            };

            col += 1;
        }
    }

    // for (i, s) in stacks.iter().enumerate() {
    //     println!("Stack {}\n{:?}", i, String::from_utf8(s.clone()));
    // }

    let mut moves = Vec::new();
    let mut iter = lines_iter
        // flat_map flattens the iterators it produces
        // same as map(f).flatten()
        // (for maps that produce iterators instead of values; Iter of Iterators -> flatten -> Iter)
        .flat_map(|ln| ln.split_whitespace()
             // test if numeric starting char -> parse as u8
            .flat_map(|s| if s.chars().next().unwrap().is_numeric()
                     // .ok() would just discard err and convert to Option
                     { Some(s.parse::<u8>().expect("should not fail") ) } else { None }));
    // keep looping while we get Some value from the iterator
    while let Some(num) = iter.next() {
        moves.push(Move {
            num: num,
            // make src/dest 0-based
            src: iter.next().expect("should be in trios") - 1,
            dest: iter.next().expect("should be in trios") - 1,
        });
    }

    // println!("Moves\n{:?}", moves);

    {
        let mut stacks_copy = stacks.clone();
        for m in moves.iter() {
            for _ in 0..m.num {
                let to_move = stacks_copy[m.src as usize].pop().unwrap();
                stacks_copy[m.dest as usize].push(to_move);
            }
        }

        print_top_crates(&stacks_copy);
    }

    {
        let mut stacks_copy = stacks.clone();
        for m in moves.iter() {
            let src = &mut stacks_copy[m.src as usize];
            let start_idx = src.len() - (m.num as usize);
            // drain -> removes item range in bulk returning an iterator
            // -> need to collect otherwise we borrow stacks_copy mutably 2x
            // could use split_off and extend instead
            let mut to_move = src.drain(start_idx..).collect();
            stacks_copy[m.dest as usize].append(&mut to_move);
        }

        print_top_crates(&stacks_copy);
    }

    Ok(())
}

#[test]
fn test_rev() {
    let mut v = vec![b'D', b'C', b'M'];
    v.reverse();
    assert!(v == [b'M', b'C', b'D'])
}

#[test]
fn test_rev_nested() {
    let mut v = vec![
        vec![b'D', b'C', b'M'],
        vec![b'N', b'Z'],
        vec![b'X', b'Y', b'Z', b'A'],
    ];

    for vv in &mut v {
        vv.reverse();
    }

    assert!(v[0] == [b'M', b'C', b'D']);
    assert!(v[1] == [b'Z', b'N']);
    assert!(v[2] == [b'A', b'Z', b'Y', b'X']);
}
