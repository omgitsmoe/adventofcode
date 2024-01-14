package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

func main() {
    file, err := os.Open("d14_input.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // use byte slice so we don't have to deal with runes etc.
    grid := make([][]byte, 0)
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        if len(line) == 0 {
            continue
        }
        grid = append(grid, []byte(line))
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }

    part1_grid := copyGrid(grid)
    // printGrid(part1_grid)
    tiltNorth(part1_grid)
    // fmt.Println("after tilt:")
    // printGrid(part1_grid)
    load := computeLoad(part1_grid)
    fmt.Println("Part1:", load)

    // rounded rock formation to first cycle it appeared at
    seen := make(map[string]int)
    const cycleCount = 1000000000
    cycle := 0
    jumped := false
    for cycle != (cycleCount - 1) {
        for _, dir := range [4]TiltTowards{North, West, South, East} {
            switch dir {
            case North:
                tiltNorth(grid)
            case West:
                tiltWest(grid)
            case South:
                tiltSouth(grid)
            case East:
                tiltEast(grid)
            }
        }

        if !jumped {
            key := makeKey(grid)
            firstCycle, found := seen[key]
            if found {
                // determine how many cycles we have left
                remainingCycles := cycleCount - (cycle - 1)
                // starting at firstCycle the cycles repeat after
                // `cyclesRepeatAfter` cycles
                cyclesRepeatAfter := cycle - firstCycle
                fmt.Println(
                    "Cycle detected at first", firstCycle, "cur", cycle,
                    "repeat", cyclesRepeatAfter)
                // and how many of those we can skip using the detected cycle point
                jumpByCycles :=
                    (remainingCycles / cyclesRepeatAfter) * cyclesRepeatAfter;
                cycle += jumpByCycles
                // so we don't jump again using wrong cycle counts etc.
                jumped = true
                continue
            } else {
                seen[key] = cycle
            }
        }
        cycle += 1
    }

    load_part2 := computeLoad(grid)
    fmt.Println("Part2:", load_part2)
}

func copyGrid(grid [][]byte) [][]byte {
    duped := make([][]byte, len(grid))
    for i := range grid {
        duped[i] = make([]byte, len(grid[i]))
        copy(duped[i], grid[i])
    }

    return duped
}

func printGrid(grid [][]byte) {
    for _, l := range grid {
        fmt.Println(string(l))
    }
}

func tiltNorth(grid [][]byte) {
    // keep track of last free spot per col
    lastFreeRow := make([]int, len(grid[0]))
    for y, row := range grid {
        for x, c := range row {
            switch c {
            case byte('O'):
                // place at last free spot
                replaceRow := lastFreeRow[x]
                if replaceRow != y {
                    grid[replaceRow][x] = byte('O')
                    row[x] = byte('.')
                }
                // new free spot
                lastFreeRow[x] += 1
            case byte('.'):
            case byte('#'):
                // blocks rounded rocks so new free spot is one beyond it
                lastFreeRow[x] = y + 1
            default:
                err := fmt.Sprintf("unexepected char %c", c)
                panic(err)
            }
        }
    }
}

func computeLoad(grid [][]byte) int {
    result := 0
    maxRows := len(grid)
    for y, row := range grid {
        for _, c := range row {
            if c == byte('O') {
                result += maxRows - y
            }
        }
    }

    return result
}

type TiltTowards int
const (
    North = iota
    West
    South
    East
)

// decided it's not worth to make this general (complexity/debugging wise)
// NOTE: would prob be better to just track positions in a map, where
//       we don't even store '.', then tilting directions also becomes easier
func tiltSouth(grid [][]byte) {
    // keep track of last free spot per col
    maxRowIdx := len(grid) - 1
    lastFreeRow := make([]int, len(grid[0]))
    for x := range lastFreeRow {
        lastFreeRow[x] = maxRowIdx
    }

    for offset_y := range grid {
        y := maxRowIdx - offset_y
        row := grid[y]
        for x, c := range row {
            switch c {
            case byte('O'):
                // place at last free spot
                replaceRow := lastFreeRow[x]
                // fmt.Println("replRow", replaceRow, "for col", x)
                if replaceRow != y {
                    grid[replaceRow][x] = byte('O')
                    row[x] = byte('.')
                }
                lastFreeRow[x] -= 1
            case byte('.'):
            case byte('#'):
                lastFreeRow[x] = y - 1
                // fmt.Println("set last free row", lastFreeRow[x], "col", x)
            default:
                err := fmt.Sprintf("unexepected char %c", c)
                panic(err)
            }
        }
    }
}

func tiltEast(grid [][]byte) {
    // keep track of last free spot per row
    maxColIdx := len(grid[0]) - 1
    lastFreeCol := make([]int, len(grid))
    for x := range lastFreeCol {
        lastFreeCol[x] = maxColIdx
    }

    for y, row := range grid {
        for offset_x := range row {
            x := maxColIdx - offset_x
            c := row[x]
            switch c {
            case byte('O'):
                // place at last free spot
                replaceCol := lastFreeCol[y]
                if replaceCol != x {
                    grid[y][replaceCol] = byte('O')
                    row[x] = byte('.')
                }
                lastFreeCol[y] -= 1
            case byte('.'):
            case byte('#'):
                lastFreeCol[y] = x - 1
            default:
                err := fmt.Sprintf("unexepected char %c", c)
                panic(err)
            }
        }
    }
}

func tiltWest(grid [][]byte) {
    // keep track of last free spot per row
    lastFreeCol := make([]int, len(grid))
    for y, row := range grid {
        for x, c := range row {
            switch c {
            case byte('O'):
                // place at last free spot
                replaceCol := lastFreeCol[y]
                if replaceCol != x {
                    grid[y][replaceCol] = byte('O')
                    row[x] = byte('.')
                }
                lastFreeCol[y] += 1
            case byte('.'):
            case byte('#'):
                lastFreeCol[y] = x + 1
            default:
                err := fmt.Sprintf("unexepected char %c", c)
                panic(err)
            }
        }
    }
}

func makeKey(grid [][]byte) string {
    // don't need to sort since we always visit them in the same order
    var builder strings.Builder
    for y, row := range grid {
        for x, c := range row {
            if c == byte('O') {
                builder.WriteString(fmt.Sprintf("%v%v|", y, x))
            }
        }
    }

    return builder.String()
}
