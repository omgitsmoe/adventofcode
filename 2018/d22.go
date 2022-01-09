package main

import (
    "fmt"
)

// package vars
const depth int = 11109
const target_x, target_y int = 9, 731
// since our shortest path can extend beyond the target add this as buffer to the 2d array
const add_x, add_y int = 1000, 1000
// const depth int = 510
// const target_x, target_y int = 10, 10

func geo_index(erosion_map *[target_y+add_y+1][target_x+add_x+1]int, x, y int) int {
    if (x == 0 && y == 0) || (x == target_x && y == target_y) {
        return 0;
    } else if  y == 0 {
        return x * 16807
    } else if  x == 0 {
        return y * 48271
    } else {
        return erosion_lvl(erosion_map, x-1, y) * erosion_lvl(erosion_map, x, y-1)
    }
}

func erosion_lvl(erosion_map *[target_y+add_y+1][target_x+add_x+1]int, x, y int) int {
    if erosion_map[y][x] != -1 {
        return erosion_map[y][x]
    } else {
        elvl := (geo_index(erosion_map, x, y) + depth) % 20183
        erosion_map[y][x] = elvl
        return elvl
    }
}

func min_cost_first(q [][4]int) {
    min := q[0][3]
    min_i := 0
    for i := 1; i < len(q); i++ {
        if q[i][3] < min {
            min = q[i][3]
            min_i = i
        }
    }
    // swap first and min
    if min_i != 0 {
        // tmp := q[0]
        // q[0] = min
        // q[min_i] = tmp
        q[0], q[min_i] = q[min_i], q[0]
    }
}

func main() {
    var erosion_map [target_y+add_y+1][target_x+add_x+1]int
    for y := range erosion_map {
        for x := range erosion_map[y] {
            erosion_map[y][x] = -1
        }
    }

    risk_sum := 0
    for y := 0; y < target_y+1; y++ {
        for x := 0; x < target_x+1; x++ {
            // 0 rocky, 1 wet, 2 narrow
            // type == risk lvl
            risk_sum += erosion_lvl(&erosion_map, x, y) % 3
        }
    }
    fmt.Println("Part1:", risk_sum)

    const ROCKY, WET, NARROW int = 0, 1, 2
    const TORCH, CLIMB, NONE int = 0, 1, 2
    // rocky, wet, narrow : tool
    // If you define the tools to be neither, torch, climbing instead then the is_valid become simply type != tool
    compat_type_tool := [3][3]bool{{true, true, false}, {false, true, true}, {true, false, true}}
    // x,y: N S W E
    directions := [4][2]int{{0, -1}, {0, 1}, {-1, 0}, {1, 0}}
    // Dijkstra algorithm (is a type of breadth-first search) vv
    // theres not pre-built queue in go so we have to use slice tricks to simulate one
    // see: https://github.com/golang/go/wiki/SliceTricks
    // x, y, tool, cost
    queue := make([][4]int, 0)
    // Push to the queue; start with mound of cave, torch 0, cost 0
    queue = append(queue, [4]int{0, 0, 0, 0})
    // x, y, tool
    visited := make(map[[3]int]bool)
    // could use this instead of my custom function that just swaps the element
    // with the smallest cost to the front; but would prob be way slower
    // sort.Slice(queue, func(i, j int) bool { return queue[i][2] < queue[j][2] })
    var current [4]int
    for {
        if len(queue) == 0 {
            break
        }
        // swaps the element with the smallest cost to the front
        min_cost_first(queue)
        // pop first element which has minimal cost (-> dijkstra)
        current, queue = queue[0], queue[1:]
        // target reached with torch
        if current[0] == target_x && current[1] == target_y && current[2] == TORCH {
            fmt.Println("Part2:", current[3])
            break
        }
        // value, present/in map
        _, prs := visited[[3]int{current[0], current[1], current[2]}]
        if prs {
            continue
        }
        // queue switching tools if compatible with current occupied field
        for t := 0; t < 3; t++ {
            // could use sep function for this but erosion lvl to type is just % 3
            if !compat_type_tool[erosion_lvl(&erosion_map, current[0], current[1]) % 3][t] {
                continue
            }
            // switching tools costs 7mins
            queue = append(queue, [4]int{current[0], current[1], t, current[3] + 7})
        }
        // queue switching to adjacent fields if tool is compatible
        for _, dxy := range directions {
            new_x, new_y := current[0] + dxy[0], current[1] + dxy[1]
            if new_x < 0 || new_y < 0 {
                // out of bounds
                continue
            } else if !compat_type_tool[erosion_lvl(&erosion_map, new_x, new_y) % 3][current[2]] {
                // cant go to adjacent field with current tool
                continue
            }
            // moving to adjacent field costs 1min, same tool
            queue = append(queue, [4]int{new_x, new_y, current[2], current[3] + 1})
        }
        // mark xy with current tool as visited
        visited[[3]int{current[0], current[1], current[2]}] = true
    }
}
