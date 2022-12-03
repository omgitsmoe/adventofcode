package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
)

func main() {
    b, err := os.Open("d02.in")
    if err != nil {
        fmt.Println(err)
    }

    scanner := bufio.NewScanner(b)
    points := 0;
    points2 := 0;
    // iterate over lines
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        if len(ln) == 0 {
            continue;
        }

        opponent := ln[0] - 'A'
        me := ln[2] - 'X'
        // see cpp for explanation
        // no ternary :/
        // needed since % in go is remainder and not modulo so -2%3 == 2 instead of 1
        var outcome int  // extra var needed since := in if doesn't leak
        // go does not panic on over/underflow -> convert byte to int
        if diff := int(me) - int(opponent) + 1; diff < 0 {
            outcome = diff + 3
        } else {
            outcome = diff % 3
        }
        // fmt.Println(ln);
        // fmt.Println("O ", outcome, "op ", opponent, "me ", me, "oidx ", outcome_idx);

        points += outcome * 3
        points += int(me) + 1
        
        // part2
        needed_outcome := int(me)
        var needed_me int
        if outcome_shift := int(opponent) - 1 + needed_outcome; outcome_shift < 0 {
            needed_me = outcome_shift + 3
        } else {
            needed_me = outcome_shift % 3
        }

        points2 += needed_outcome * 3 + needed_me + 1
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }

    fmt.Println("Part1: Total points are ", points)
    fmt.Println("Part2: Total points are ", points2)
}

