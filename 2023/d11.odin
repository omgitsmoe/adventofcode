package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:mem"

Galaxy :: struct {
    x, y: int,
}

sum_galaxy_dists :: proc(lines: [dynamic]string, empty_offset: int) -> int {
    has_galaxy_row := make([dynamic]bool, len(lines))
    defer delete(has_galaxy_row)
    has_galaxy_col := make([dynamic]bool, len(lines[0]))
    defer delete(has_galaxy_col)

    for line, row in lines {
        for c, col in line {
            if c == '#' {
                has_galaxy_row[row] = true
                has_galaxy_col[col] = true
            }
        }
    }

    galaxies := make([dynamic]Galaxy)
    defer delete(galaxies)
    y := 0
    x := 0
    // iterate over the rows/cols counting the idx ourselves so we can
    // add 2 if a row/col didn't have any galaxy
    // -> we don't have to append any lines/characters
    for line, row in lines {
        x = 0
        for c, col in line {
            if c == '#' {
                append(&galaxies, Galaxy{y = y, x = x})
            }

            if has_galaxy_col[col] {
                x += 1
            } else {
                x += 1 + empty_offset
            }
        }

        if has_galaxy_row[row] {
            y += 1
        } else {
            y += 1 + empty_offset
        }
    }

    dist_sums := 0
    for g1, g1_idx in galaxies {
        // start at current idx + 1 so we don't count double
        for g2 in galaxies[g1_idx + 1:] {
            // shortest path is just the manhattan distance
            manhattan_dist := abs(g1.y - g2.y) + abs(g1.x - g2.x)
            dist_sums += manhattan_dist
        }
    }

    return dist_sums
}

main :: proc() {
    // setup up tracking allocator to make sure we free all allocations
    // (doesn't matter since this is a one-off thing and relying on OS
    //  to free would be fine, just used for understanding how memory
    //  management works in Odin)
    track: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track, context.allocator)
    context.allocator = mem.tracking_allocator(&track)

    defer {
        if len(track.allocation_map) > 0 {
            fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
            for _, entry in track.allocation_map {
                fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
            }
        }
        if len(track.bad_free_array) > 0 {
            fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
            for entry in track.bad_free_array {
                fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
            }
        }
        mem.tracking_allocator_destroy(&track)
    }
    // tracking allocator setup end

    data, ok := os.read_entire_file("d11_input.txt", context.allocator)
    if !ok {
        fmt.println("Failed to read file!")
        os.exit(1)
    }
    defer delete(data, context.allocator)

    // not a copy, just a cast/alias, since data is []u8
    input := string(data)
    lines := make([dynamic]string)
    defer delete(lines)
    // NOTE: !!!! strings.*_iterator __consumes__ the input string
    //       (as in moves the ptr forward and reduces the len)
    for line in strings.split_lines_iterator(&input) {
        if len(line) > 0 {
            append(&lines, line)
        }
    }

    dist_sums := sum_galaxy_dists(lines, 1)
    fmt.println("Part1:", dist_sums)
    // NOTE: task is to make it 1mio __times larger__ not add 1mio
    dist_sums2 := sum_galaxy_dists(lines, 1000000 - 1)
    fmt.println("Part2:", dist_sums2)
}
