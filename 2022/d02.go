package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
)

const (
    LOSE int = 0
    DRAW     = 3
    WIN      = 6
)

func get_option(c byte) byte {
    switch c {
    case 'A', 'X':
        return 1;
    case 'B', 'Y':
        return 2;
    case 'C', 'Z':
        return 3;
    }

    panic("Unexpected")
}

func main() {
    b, err := os.Open("d02.in")
    if err != nil {
        fmt.Println(err)
    }

    // no constant array in go :/
    var OUTCOMES = []int{DRAW, LOSE, WIN};

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

        opponent := get_option(ln[0])
        me := get_option(ln[2])
        // no ternary :/
        // needed since % in go is remainder and not modulo so -2%3 == 2 instead of 1
        var outcome_idx int  // extra var needed since := in if doesn't leak
        // go does not panic on over/underflow -> convert byte to int
        if diff := int(opponent) - int(me); diff < 0 {
            outcome_idx = diff + 3
        } else {
            outcome_idx = diff % 3
        }
        outcome := OUTCOMES[outcome_idx]
        // fmt.Println(ln);
        // fmt.Println("O ", outcome, "op ", opponent, "me ", me, "oidx ", outcome_idx);

        points += outcome
        points += int(me)
        
        // part2
        var needed_outcome int
        var outcome_shift int
        switch ln[2] {
        case 'X':
            needed_outcome = LOSE
            // need opponent to WIN
            outcome_shift = 2
        case 'Y':
            needed_outcome = DRAW
            outcome_shift = 0
        case 'Z':
            needed_outcome = WIN
            // need opponent to LOSE
            outcome_shift = 1
        }

        // fine to use %3 since it can't underflow
        needed_me := 1 + (int(opponent) + outcome_shift - 1) % 3
        points2 += needed_outcome + needed_me
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }

    fmt.Println("Part1: Total points are ", points)
    fmt.Println("Part2: Total points are ", points2)
}

