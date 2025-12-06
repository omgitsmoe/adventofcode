app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }

import cli.Stdout
import cli.File

Op: [Unknown, Add, Mul]
Problem: {numbers: List I64, operator: Op}

# NOTE: gave up Roc is still unusable, same as it was in 2023
#       parsing errors, compiler freezes, crashes and more... yay
main! = |_args|
    # NOTE: indent important! (similar to F#)
    input =
        File.read_utf8!("d06.example")?
        |> Str.split_on("\n")
        |> List.map(|line|
            line
                |> Str.split_on(" ")
                |> List.drop_if(|s| Str.is_empty s)
        )
        |> List.drop_if(|l| l |> List.is_empty)

    # _ = content_str
    #     |> List.walk!([], |acc, line|
    #         _ = Stdout.line!(line)
    #         acc
    #     )
    dbg build_problems(input)

    Ok({})


# NOTE: not documented anywhere but List is a dynamic array,
#       not a linked list, so appends are fast and prepends
#       need to move all elements at/after the insertion point
#       https://github.com/roc-lang/roc/blob/alpha4-rolling/src/builtins/list.zig#L646
#       https://github.com/roc-lang/roc/blob/90441419601305db1dc3638a0c956240efee9533/crates/compiler/builtins/roc/List.roc
build_problems = |rows|
    numbers_count = (rows |> List.len) - 1  # -1 for operator
    problems_count = rows |> List.first? |> List.len
    initial = List.repeat({numbers: [], operator: Unknown}, problems_count)
    # compiler crash in ir.rs
    # Ok(rows
    #     |> List.walk(initial, |acc, num_or_op_str| acc))
    # same for
    # rows
    #     |> List.walk(initial, |acc, num_or_op_str| acc)
    #     |> Ok
    # same for
    # _ = rows
    #     |> List.walk(initial, |acc, num_or_op_str| acc)
    # Ok([])
    # => gave up... again
    # unfinished "paren"  v -> unusable in this state
    rows
    |> List.walk(initial, |acc, row|
        # (_, acc) = List.walk((todo, done), |acc, num_or_op_str|
        #     head, tail = when todo is
        #         [] ->  crash
        #         [head] -> (head, [])
        #         [head, .. as tail] -> (head, tail)

        #     head = when num_or_op_str is
        #         "*" -> {head & operator: Mul}
        #         "+" -> {head & operator: Add}
        #         s -> {head & numbers: head.numbers |> List.append(Str.to_i64(s))}

        #     (tail, done |> List.append(head))
        # )

        # acc
    )

