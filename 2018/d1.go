package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
    "strconv"
)

func main() {
    b, err := os.Open("d1_input.txt")
    if err != nil {
        fmt.Println(err)
    }
    freq := 0
    var nrs [1000]int
    lines_i := 0
    scanner := bufio.NewScanner(b)
    // iterate over lines
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        nr, err := strconv.Atoi(ln)
        if err != nil {
            fmt.Println(err)
        }
        freq += nr
        nrs[lines_i] = nr
        lines_i++
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }
    fmt.Println("Part1:", freq)

    // part2
    freq = 0
    // works but takes forever with normal array
    // var seen []int
    // if we pass a capacity the default zero val will be used for the type
    // int -> 0 and we have to set the elements manually up
    // to the capacity
    // seen = make([]int, 0)
    seen := make(map[int]bool)
// label for breaking out of outer loop
repeat_list:
    for {
        // iterate over lines
        for _, nr := range nrs {
            seen[freq] = true
            freq += nr
            _, present := seen[freq]
            if present {
                fmt.Println("Part2:", freq)
                break repeat_list
            }
        }
    }
}
