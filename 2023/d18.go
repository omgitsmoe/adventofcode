package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
    "strconv"
    "math"
)

type Vec struct {
    y, x int
}

type Instruction struct {
    direction Vec
    meters int
    color []byte
}

type Range struct {
    min, max int
}

func direction(letter byte) Vec {
    switch letter {
    case byte('U'):
        return Vec{y: -1, x: 0}
    case byte('R'):
        return Vec{y: 0, x: 1}
    case byte('D'):
        return Vec{y: 1, x: 0}
    case byte('L'):
        return Vec{y: 0, x: -1}
    }
    panic("unmatched direction letter")
}

func main() {
    file, err := os.Open("d18_input.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // use byte slice so we don't have to deal with runes etc.
    instructions := make([]Instruction, 0)
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := []byte(scanner.Text())
        if len(line) == 0 {
            continue
        }
        var instruction Instruction
        instruction.direction = direction(line[0])

        meters_end_exclusive := strings.LastIndexByte(
            string(line), byte(' '))
        meters, err := strconv.Atoi(
            string(line[2:meters_end_exclusive]))
        if err != nil {
            panic(err)
        }
        instruction.meters = meters
        instruction.color = line[meters_end_exclusive + 3:]

        instructions = append(instructions, instruction)
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }

    // NOTE: islands can exist like in the pipe puzzle of a previous day
    // NOTE: use the shoelace formula (see
    //       https://11011110.github.io/blog/2021/04/17/picks-shoelaces.html,
    //       https://www.101computing.net/the-shoelace-algorithm/)
    //       for calculating the area of a non-overlapping polygon
    //       (there's also pick's theorem, but there you need the nr of inner
    //        and boundary points which is less useful here)
    //       __assumes__ that the vertices are visited in clockwise direction
    //       such that the interior points lie right from the travel direction
    //       A= \frac{1}{2}\sum_{(x,y)\to (x',y')}(x'-x)(y'+y).
    vertices := vertices_from_instructions(instructions)
        // old
        // You can't change values associated with keys in a map, you can
        // only reassign values.
        // -> make a copy then reassign
        // (other solution would be storing pointers instead)
        // minmax, found := rowToRange[pos.y]

    // fmt.Println("verts:", vertices)
    // NOTE: shoelace does not count the boundary points, since in a regular
    // polygon the lines between vertices don't have a size
    // so calculate the complete result using Pick's theorem
    // (shoelace gives us the __area__ (not the inside points))
    fmt.Println("Part1:", picks_theorem(boundary(vertices), shoelace(vertices)))
    // https://rosettacode.org/wiki/Shoelace_formula_for_polygonal_area
    if !(shoelaceCw([]Vec{
        Vec{y: 4, x: 3},
        Vec{y: 11, x: 5},
        Vec{y: 8, x: 12},
        Vec{y: 5, x: 9},
        Vec{y: 6, x: 5},
    }) == 30) {
        panic("shoelaceCw func wrong")
    }
    // https://www.101computing.net/the-shoelace-algorithm/
    if !(shoelaceCcw([]Vec{
        Vec{y: 7, x: 2},
        Vec{y: 1, x: 10},
        Vec{y: 6, x: 8},
        Vec{y: 7, x: 11},
        Vec{y: 10, x: 7},
    }) == 32) {
        panic("shoelaceCcw func wrong")
    }
    
    decoded_instructions := make([]Instruction, len(instructions))
    for i, instruction := range instructions {
        decoded := instruction
        meters, err := strconv.ParseInt(string(instruction.color[0:5]), 16, 32)
        if err != nil {
            panic(err)
        }
        decoded.meters = int(meters)
        var dir_letter byte
        switch instruction.color[5] {
        case byte('0'):
            dir_letter = byte('R')
        case byte('1'):
            dir_letter = byte('D')
        case byte('2'):
            dir_letter = byte('L')
        case byte('3'):
            dir_letter = byte('U')
        }
        decoded.direction = direction(dir_letter)
        decoded_instructions[i] = decoded
    }
    decoded_vertices := vertices_from_instructions(decoded_instructions)
    fmt.Println("Part2:",
        picks_theorem(
            boundary(decoded_vertices),
            shoelace(decoded_vertices)))
}

func vertices_from_instructions(instructions []Instruction) []Vec {
    vertices := make([]Vec, 0)
    pos := Vec{y: 0, x: 0}
    // starting pos is already dug out
    // we end back on start, so we don't have to add it
    // vertices = append(vertices, pos)
    for _, instruction := range instructions {
        pos = Vec{y: pos.y + instruction.direction.y * instruction.meters,
                  x: pos.x + instruction.direction.x * instruction.meters}
        vertices = append(vertices, pos)
    }
    return vertices
}

func shoelace(vertices []Vec) int {
    // NOTE: do both cw and ccw so we also get the correct result depending
    //       on the walking direction
    // -> result >0 is the correct one
    cw := shoelaceCw(vertices)
    ccw := shoelaceCcw(vertices)
    if cw > 0 {
        return cw
    } else {
        return ccw
    }
}

func picks_theorem(num_boundary, area int) int {
    // picks theorem (https://11011110.github.io/blog/2021/04/17/picks-shoelaces.html)
    // A=i+b/2-1.
    // return num_inside + num_boundary / 2 - 1
    // this calculates the area, but we want
    // I+B = A + B/2 + 1
    return area + num_boundary / 2 + 1
}

func boundary(vertices []Vec) int {
    boundary_points := 0
    for i := range vertices {
        j := (i + 1) % len(vertices)
        pos := vertices[i]
        posNext := vertices[j]
        boundary_points += int(math.Abs(float64(posNext.y - pos.y)) + math.Abs(float64(posNext.x - pos.x)))
    }
    
    return boundary_points
}

func shoelaceCcw(vertices []Vec) int {
    // visit in counter-clockwise order
    sumLeft := 0
    sumRight := 0
    for i := range vertices {
        j := (i + 1) % len(vertices)
        pos := vertices[i]
        posNext := vertices[j]
        sumLeft += pos.x * posNext.y
        sumRight += pos.y * posNext.x
    }
    area := int(math.Abs(float64(sumLeft - sumRight)) / 2)
    return area;
}

func shoelaceCw(vertices []Vec) int {
    // visit in clockwise order
    sum := 0
    pos := vertices[len(vertices) - 1]
    for _, posNext := range vertices {
        // for i := range vertices {
        // j := (i + 1) % len(vertices)
        // pos := vertices[i]
        // posNext := vertices[j]
        // sum += (posNext.x - pos.x) * (posNext.y + pos.y)
        // equivalent to ^^
        sum += (pos.y * posNext.x) - (pos.x * posNext.y)
        pos = posNext
    }
    return sum / 2;
}
