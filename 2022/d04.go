package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
    "strconv"
)

type Range struct {
    start int
    end_inclusive int
}

func (a Range) contains(b Range) bool {
    var ret = false
    if a.start <= b.start &&
            a.start <= b.end_inclusive &&
            a.end_inclusive >= b.end_inclusive {
        ret = true
    }

    return ret
}

func (a Range) overlaps(b Range) bool {
    var ret = true
    if a.start > b.end_inclusive {
        // starts after b
        ret = false
    } else if a.start < b.start && a.end_inclusive < b.start {
        // start+ends before b
        ret = false
    }

    return ret
}

func range_from_str(s string) Range {
    dash_idx := strings.Index(s, "-");
    start, err := strconv.Atoi(s[:dash_idx])
    if err != nil {
        panic(err)
    }
    end_inclusive, err := strconv.Atoi(s[dash_idx + 1:])
    if err != nil {
        panic(err)
    }

    return Range { 
        start, end_inclusive,
    }
}

func main() {
    b, err := os.Open("d04.in")
    if err != nil {
        fmt.Println(err)
    }

    scanner := bufio.NewScanner(b)
    fully_contained := 0
    overlapping := 0
    // iterate over lines
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        if len(ln) == 0 {
            continue;
        }

        comma_idx := strings.Index(ln, ",")
        r1_str := ln[:comma_idx];
        r2_str := ln[comma_idx + 1:];
        range1 := range_from_str(r1_str)
        range2 := range_from_str(r2_str)
        if range1.contains(range2) || range2.contains(range1) {
            fully_contained += 1
        }
        if range1.overlaps(range2) {
            overlapping += 1
        }
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }

    fmt.Println("Part1: Fully contained ranges", fully_contained)
    fmt.Println("Part2: Overlapping ranges", overlapping)
}

