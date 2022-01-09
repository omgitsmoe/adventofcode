package main

import (
    "fmt"
    "bufio"
    "os"
    "strings"
    "strconv"
)

type Claim struct {
    Id int
    PosX int
    PosY int
    Width int
    Height int
}

func main() {
    b, err := os.Open("d3_input.txt")
    if err != nil {
        fmt.Println(err)
    }
    var claims []Claim
    scanner := bufio.NewScanner(b)
    // iterate over lines
    for scanner.Scan() {
        // get line and trim whitespace
        ln := strings.TrimSpace(scanner.Text())
        split := strings.Split(ln, " ")
        id, _ := strconv.Atoi(split[0][1:])
        pos, size := split[2][:len(split[2])-1], split[3]
        split = strings.Split(pos, ",")
        x, _ := strconv.Atoi(split[0])
        y, _ := strconv.Atoi(split[1])
        split = strings.Split(size, "x")
        width, _ := strconv.Atoi(split[0])
        height, _ := strconv.Atoi(split[1])
        claim := Claim{id, x, y, width, height}
        claims = append(claims, claim)
    }
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading from file input:", err)
    }

    var fabric [1000][1000][]int
    for _, claim := range claims {
        for y := 0; y < claim.Height; y++ {
            for x := 0; x < claim.Width; x++ {
                // The reference to the underlying array is a value, and in Go,
                // everything is passed by value (even pointers/references). So
                // your function that receives a slice is receiving a copy of
                // the slice header. The slice header contains important
                // information like starting address and size. When you append
                // to the slice in your other function, the copy of the slice
                // header gets modified, but the original calling function
                // doesn't see that copy, it still has its own. That's why
                // functions like append return the new value, which is the
                // modified slice header.
                current := fabric[claim.PosY+y][claim.PosX+x]
                fabric[claim.PosY+y][claim.PosX+x] = append(current, claim.Id)
            }
        }
    }


    overlapping := 0
    // part2
    overlap := make(map[int]bool)
    for row := range fabric {
        for col := range fabric[row] {
            if len(fabric[row][col]) > 1 {
                overlapping++
                for _, id := range fabric[row][col] {
                    overlap[id] = true
                }
            }
        }
    }
    fmt.Println("Part1:", overlapping)
    for i := 0; i < len(claims); i++ {
        // map returns zero default (here: false) if key not present
        if !overlap[i+1] {
            fmt.Println("Part2:", i+1)
            break
        }
    }
}
