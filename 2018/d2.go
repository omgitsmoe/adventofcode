package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
)

func main() {
    b, err := os.Open("d2_input.txt")
    if err != nil {
        fmt.Println(err)
    }
    two_count := 0
    three_count := 0
    var lines []string
    scanner := bufio.NewScanner(b)
    // iterate over lines
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        lines = append(lines, ln)

        two := false
        three := false
        count := 0
        for _, c := range ln {
            count = strings.Count(ln, string(c))
            if count == 2 {
                two = true
            } else if count == 3 {
                three = true
            }
        }
        if two {
            two_count++
        }
        if three {
            three_count++
        }
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }
    fmt.Println("Part1:", two_count*three_count)

    // part2
    find_ids:
    for _, box_id := range lines {
        for _, other_id := range lines {
            diff := 0
            for i := range box_id {
                if box_id[i] != other_id[i] {
                    diff++
                    if diff > 1 {
                        break
                    }
                }
            }
            if diff == 1 {
                common := ""
                // build string with common letters
                for i := range box_id {
                    if box_id[i] == other_id[i] {
                        common += string(box_id[i])
                    }
                }
                fmt.Println("Part2:", common)
                break find_ids
            }
        }
    }
}
