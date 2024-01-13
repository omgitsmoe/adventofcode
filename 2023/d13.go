package main

import (
    "fmt"
    "os"
    "bufio"
)

func main() {
    file, err := os.Open("d13_input.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // use byte slice so we don't have to deal with runes etc.
    grids := make([][][]byte, 1)
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        if len(line) == 0 {
            grids = append(grids, make([][]byte, 0))
            continue
        }
        // assignment copies, so grid := grids[0] -> copy
        // -> take a ptr instead
        grid := &grids[len(grids) - 1]
        // then we need to deref the ptr like in C
        *grid = append(*grid, []byte(line))
    }

    // in case of empty line at end, drop last grid
    if len(grids[len(grids) - 1]) == 0 {
        grids = grids[:len(grids) - 1]
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }

    part1 := 0
    for _, g := range grids {
        // printGrid(g)
        // reflectionPoint inclusive on the lower end
        reflectionPoint, accessType, found := findReflectionPoint(g)
        if !found {
            panic("Failed to find reflectionPoint")
        }
        if accessType == ByRow {
            // add each vertical line of reflection to the left
            part1 += reflectionPoint + 1
        } else {
            // add each horizontal line of reflection above * 100
            part1 += (reflectionPoint + 1) * 100
        }
    }
    fmt.Println("Part1:", part1)

    part2 := 0
    Outer:
    for _, g := range grids {
        // printGrid(g)
        // reflectionPoint inclusive on the lower end
        originalReflectionPoints, violationPoints := findReflectionPointPart2(g)

        // test swapping the symbol at all violationPoints until we find
        // a reflection match that is different from the original
        for pos, _ := range violationPoints {
            // swap ash/rock instead of copying
            swapType(g, pos)

            foundReflectionPoints, _ := findReflectionPointPart2(g)
            // sanity check for different refelection points
            if len(foundReflectionPoints) > 1 {
                for i := range foundReflectionPoints[1:] {
                    if foundReflectionPoints[i] != foundReflectionPoints[i + 1] {
                        // error if it's not the original one
                        if foundReflectionPoints[i] != originalReflectionPoints[0] && foundReflectionPoints[i+1] != originalReflectionPoints[0] {
                            panic("mult distinct refl points")
                        }
                    }
                }
            }
            
            for _, refl := range foundReflectionPoints {
                // ignore originalReflectionPoint
                if (refl.idx != originalReflectionPoints[0].idx ||
                        refl.accessType != originalReflectionPoints[0].accessType) {
                    // WRONG: since defer is function scoped :/ defer swapType(pos)

                    if refl.accessType == ByRow {
                        // add each vertical line of reflection to the left
                        part2 += refl.idx + 1
                    } else {
                        // add each horizontal line of reflection above * 100
                        part2 += (refl.idx + 1) * 100
                    }
                    swapType(g, pos)
                    continue Outer
                }
            }

            swapType(g, pos)
        }

        printGrid(g)
        panic("did not find other reflection point")
    }
    fmt.Println("Part2:", part2)
}

func swapType(grid [][]byte, pos Pos) {
    s := grid[pos.row][pos.col]
    if s == byte('#') {
        grid[pos.row][pos.col] = byte('.')
        // fmt.Println("Swapped r", pos.row, "c", pos.col, s, "to", ".")
    } else {
        grid[pos.row][pos.col] = byte('#')
        // fmt.Println("Swapped r", pos.row, "c", pos.col, s, "to", "#")
    }
}

func printGrid(grid [][]byte) {
    for _, l := range grid {
        fmt.Println(string(l))
    }
}

// distinct type
// soo func foo(a AccessType)
// errors when
// var a int = 0
// foo(a) -> error
// BUT
// foo(0) -> same underlying type so automatically converted
// => no error
type AccessType int
const (
    // names are a little confusing
    // ByRow -> trying to match row starting at a certain col idx
    //          (so reflectionPoint is actually the col idx)
    // ByCol just reversed
    ByRow AccessType = 0
    ByCol AccessType = 1
)

func match(grid [][]byte, accessType AccessType, fixedIdx, varIdxA, varIdxB int) (bool, int, int) {
    var access func (idx int) byte
    var maxlen int
    // helper so we can reuse this function for both row/col matching
    if accessType == ByRow {
        // fixedIdx = row
        maxlen = len(grid[fixedIdx])
        access = func(idx int) byte {
            return grid[fixedIdx][idx]
        }
    } else {
        // fixedIdx = col
        maxlen = len(grid)
        access = func(idx int) byte {
            return grid[idx][fixedIdx]
        }
    }
    // assumes varIdxA < varIdxB
    for {
        if varIdxA < 0 || varIdxB >= maxlen {
            break
        }
        if access(varIdxA) != access(varIdxB) {
            // return non-matching index
            return false, varIdxA, varIdxB
        }
        varIdxA -= 1
        varIdxB += 1
    }

    return true, varIdxA, varIdxB
}

func findReflectionPoint(grid [][]byte) (int, AccessType, bool) {
    {
        // find possible reflection points
        // -> needs two consecutive matching symbols for the start point
        possibleReflectionPoints := make([]int, 1)
        for reflectionPoint, c1 := range grid[0] {
            if reflectionPoint == len(grid[0]) - 1 {
                break
            }
            if c1 == grid[0][reflectionPoint + 1] {
                possibleReflectionPoints = append(
                    possibleReflectionPoints, reflectionPoint)
            }
        }

        Outer:
        for _, possibleReflectionPoint := range possibleReflectionPoints {
            colA, colB := possibleReflectionPoint, possibleReflectionPoint + 1
            // match each row starting at the col reflectionPoint
            for row := range grid {
                if m, _, _ := match(grid, ByRow, row, colA, colB); !m {
                    continue Outer
                }
            }

            // all rows matched at the reflectionPoint -> done
            return possibleReflectionPoint, ByRow, true
        }
    }

    {
        possibleReflectionPoints := make([]int, 1)
        for reflectionPoint := range grid {
            if reflectionPoint == len(grid) - 1 {
                break
            }
            r1 := grid[reflectionPoint][0]
            r2 := grid[reflectionPoint + 1][0]
            if r1 == r2 {
                possibleReflectionPoints = append(
                    possibleReflectionPoints, reflectionPoint)
            }
        }

        OuterCol:
        for _, possibleReflectionPoint := range possibleReflectionPoints {
            rowA, rowB := possibleReflectionPoint, possibleReflectionPoint + 1
            // match each col starting at the row reflectionPoint
            for col := range grid[0] {
                if m, _, _ := match(grid, ByCol, col, rowA, rowB); !m {
                    continue OuterCol
                }
            }

            // all cols matched at the reflectionPoint -> done
            return possibleReflectionPoint, ByCol, true
        }
    }

    return 0, 0, false
}

// decided to swap the symbol back/forth instead to save time
func copyGrid(grid [][]byte) [][]byte {
    duped := make([][]byte, len(grid))
    for i := range grid {
        duped[i] = make([]byte, len(grid[i]))
        copy(duped[i], grid[i])
    }

    return duped
}

type Pos struct {
    row, col int
}

type ReflectionPoint struct {
    idx int
    accessType AccessType
}

func findReflectionPointPart2(grid [][]byte) ([]ReflectionPoint, map[Pos]bool) {

    // keep track of points that messed up the reflection
    // -> only need the first point
    // -> __unless__ the rows/cols were ignored (since 0/end reached in comparison)
    violationPoints := make(map[Pos]bool)
    // now we also can find multiple possibleReflectionPoints
    foundReflectionPoints := make([]ReflectionPoint, 0)
    {
        // find possible reflection points
        // -> needs two consecutive matching symbols
        possibleReflectionPoints := make([]int, 1)
        for reflectionPoint, c1 := range grid[0] {
            if reflectionPoint == len(grid[0]) - 1 {
                break
            }
            if c1 == grid[0][reflectionPoint + 1] {
                possibleReflectionPoints = append(
                    possibleReflectionPoints, reflectionPoint)
            }
        }

        Outer:
        for _, possibleReflectionPoint := range possibleReflectionPoints {
            colA, colB := possibleReflectionPoint, possibleReflectionPoint + 1
            for row := range grid {
                if m, mismatchColA, mismatchColB := match(grid, ByRow, row, colA, colB); !m {
                    violationPoints[Pos{row, mismatchColA}] = true
                    violationPoints[Pos{row, mismatchColB}] = true
                    continue Outer
                } else {
                    // we also need to try swapping ignored rows/cols
                    // and not just the first one!!
                    if mismatchColA - 1 >= 0 {
                        for c := mismatchColA - 1; c >= 0; c-- {
                            violationPoints[Pos{row, c}] = true
                        }
                    }
                    if mismatchColB + 1 < len(grid[0]) {
                        for c := mismatchColB + 1; c < len(grid[0]); c++ {
                            violationPoints[Pos{row, c}] = true
                        }
                    }
                }
            }

            // all rows matched at the reflectionPoint -> done
            foundReflectionPoints = append(
                foundReflectionPoints,
                ReflectionPoint{ possibleReflectionPoint, ByRow })
        }
    }

    {
        possibleReflectionPoints := make([]int, 1)
        for reflectionPoint := range grid {
            if reflectionPoint == len(grid) - 1 {
                break
            }
            r1 := grid[reflectionPoint][0]
            r2 := grid[reflectionPoint + 1][0]
            if r1 == r2 {
                possibleReflectionPoints = append(
                    possibleReflectionPoints, reflectionPoint)
            }
        }

        OuterCol:
        for _, possibleReflectionPoint := range possibleReflectionPoints {
            rowA, rowB := possibleReflectionPoint, possibleReflectionPoint + 1
            for col := range grid[0] {
                if m, mismatchRowA, mismatchRowB := match(grid, ByCol, col, rowA, rowB); !m {
                    violationPoints[Pos{mismatchRowA, col}] = true
                    violationPoints[Pos{mismatchRowB, col}] = true
                    continue OuterCol
                } else {
                    // we also need to try swapping ignored rows/cols
                    // and not just the first one!!
                    if mismatchRowA - 1 >= 0 {
                        for r := mismatchRowA - 1; r >= 0; r-- {
                            violationPoints[Pos{r, col}] = true
                        }
                    }
                    if mismatchRowB + 1 < len(grid) {
                        for r := mismatchRowB + 1; r < len(grid); r++ {
                            violationPoints[Pos{r, col}] = true
                        }
                    }
                }
            }

            // all cols matched at the reflectionPoint -> done
            foundReflectionPoints = append(
                foundReflectionPoints,
                ReflectionPoint{ possibleReflectionPoint, ByCol })
        }
    }

    return foundReflectionPoints, violationPoints
}
