package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
)

func neighbours(area *[50][50]rune, y, x int) [3]int {
    var result [8]rune
    // up left to left mid (cw)
    if y > 0 {
        if x > 0 {
            result[0] = area[y - 1][x - 1]
        }
        result[1] = area[y - 1][x]
        if x < 49 {
            result[2] = area[y - 1][x + 1]
        }
    }
    if y < 49 {
        if x < 49 {
            result[4] = area[y + 1][x + 1]
        }
        result[5] = area[y + 1][x]
        if x > 0 {
            result[6] = area[y + 1][x - 1]
        }
    }
    if x < 49 {
        result[3] = area[y][x + 1]
    }
    if x > 0 {
        result[7] = area[y][x - 1]
    }
    // open, tree, lumberyard
    var count [3]int
    for i := range result {
        if result[i] == '.' {
            count[0]++
        } else if result[i] == '|' {
            count[1]++
        } else if result[i] == '#' {
            count[2]++
        }
    }
    return count
}


func main() {
    b, err := os.Open("d18.in")
    if err != nil {
        fmt.Println(err)
    }
    var area [50][50]rune
    scanner := bufio.NewScanner(b)
    // iterate over lines
    // Scan advances the Scanner to the next token, which will then be
    // available through the Bytes or Text method
    ln_i := 0
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        // iterating over a string in go with range will give us the byte index at which
        // the rune starts and optionally the rune itself
        // but here we need the actual index of the char not its byte index
        i := 0
        for _, c := range ln {
            area[ln_i][i] = c
            i++
        }
        ln_i++
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }
    // problems/simulations like the are called cellular automata
    var cycle [28][2]int
    var new_area [50][50]rune
    for i := 0; i < 1000000000; i++ {
        for y := range area {
            for x := range area[y] {
                current := area[y][x]
                count := neighbours(&area, y, x)
                // fmt.Printf("%c\n", area[y][x])
                // fmt.Println(count)
                if current == '.' {
                    if count[1] > 2 {
                        new_area[y][x] = '|'
                    } else {
                        new_area[y][x] = '.'
                    }
                } else if current == '|' {
                    if count[2] > 2 {
                        new_area[y][x] = '#'
                    } else {
                        new_area[y][x] = '|'
                    }
                } else if current == '#'{
                    if count[1] > 0 && count[2] > 0 {
                        new_area[y][x] = '#'
                    } else {
                        new_area[y][x] = '.'
                    }
                }
                // fmt.Printf("%c\n", new_area[y][x])
                // fmt.Printf("=========================\n")
            }
        }
        area = new_area
        trees := 0
        lumberyards := 0
        for y := range area {
            for x := range area[y] {
                current := area[y][x]
                // fmt.Printf("%c", current)
                if current == '|' {
                    trees++
                } else if current == '#' {
                    lumberyards++
                }
            }
            // fmt.Println()
        }
        // part1
        if i == 9 {
            fmt.Println("Part1:", trees * lumberyards)
        }
        // end of cycle reached
        if i == 447 {
            break
        }
        // start of cycle -> record values into cycle
        if i >= 419 {
            cycle[i - 419][0] = trees
            cycle[i - 419][1] = lumberyards
        }
    }
    // comment: found this by looking for repetition in the prints of the numbers
    // for trees, lumberyards and resources but in the solution thread it
    // is suggested to look for a repeating grid (same grid state 
    // appearing for the 2nd time) to find the cycle
    // 291 lumberyards repeats every 28 minutes starting with min 419
    // when starting with min 0
    // same for trees -> 554 trees every 28 minutes starting with
    // we want minute 1000000000 -> were at step (1000000000 % 28) in
    // the cycle
    pos_in_cycle := 1000000000 % 28
    fmt.Println("Part2:", cycle[pos_in_cycle][0] * cycle[pos_in_cycle][1])
    }
