package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
)

func mark_items(items []bool, str string) {
    for _, c := range str {
        var idx int
        if c <= 'Z' {
            idx = int(c) - 'A' + 26
        } else {
            idx = int(c) - 'a'
        }

        // fmt.Println("C", c, "->", string(c), "I", idx)
        items[idx] = true
    }
}

func main() {
    b, err := os.Open("d03.in")
    if err != nil {
        fmt.Println(err)
    }

    scanner := bufio.NewScanner(b)
    point_sum := 0;
    var lines []string
    // iterate over lines
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        lines = append(lines, ln)
        if len(ln) == 0 {
            continue;
        }

        half := len(ln) / 2;
        compartment1 := ln[:half];
        compartment2 := ln[half:];
        // initialized to 0
        var items1 [53]bool
        var items2 [53]bool
        mark_items(items1[:], compartment1)
        mark_items(items2[:], compartment2)

        for i := range items1 {
            in_both := items1[i] && items2[i]
            if in_both {
                point_sum += i + 1
            }
        }
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }

    var rucksacks [3][53]bool
    // start at 1 due to %3
    idx := 1
    common_point_sum := 0
    for _, ln := range lines {
        items := &rucksacks[idx % 3]
        mark_items(items[:], ln)

        if idx % 3 == 0 {
            for i := range rucksacks[0] {
                in_all := rucksacks[0][i] && rucksacks[1][i] && rucksacks[2][i]
                if in_all {
                    common_point_sum += i + 1
                }
            }

            // reset
            rucksacks = [3][53]bool{}
        }

        idx += 1
    }

    fmt.Println("Part1: Duplicate item prios are", point_sum)
    fmt.Println("Part2: Triple item prios are", common_point_sum)
}

