package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:mem"
import "core:strconv"
import "core:slice"

Record :: struct {
    // use []u8 instead of string, since strings are immutable
    arrangement: []u8,
    damaged_groups: []int,
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

    data, ok := os.read_entire_file("d12_input.txt", context.allocator)
    if !ok {
        fmt.println("Failed to read file!")
        os.exit(1)
    }
    defer delete(data, context.allocator)

    // no a copy, just a cast
    input := string(data)
    springs := make([dynamic]string)
    defer delete(springs)
    num_damaged := make([dynamic][dynamic]int)
    defer {
        for _, i in num_damaged {
            delete(num_damaged[i])
        }
        delete(num_damaged)
    }
    // NOTE: !!!! strings.*_iterator __consumes__ the input string
    //       (as in moves the ptr forward and reduces the len)
    for line in strings.split_lines_iterator(&input) {
        if len(line) == 0 {
            continue
        }

        springs_end_exclusive := strings.index_byte(line, ' ')
        append(&springs, line[:springs_end_exclusive])

        row_damaged := make([dynamic]int)
        damaged_springs_part := line[springs_end_exclusive + 1:]
        for num_damaged_springs_str in strings.split_iterator(
                &damaged_springs_part, ",") {
            num_damaged_springs, ok := strconv.parse_int(num_damaged_springs_str)
            if !ok {
                fmt.println("Failed to convert '", num_damaged_springs_str,
                            "' to string", sep = "")
                os.exit(1)
            }
            append(&row_damaged, num_damaged_springs)
        }

        append(&num_damaged, row_damaged)
    }

    // 2nd param to set length (otherwise len == capacity)
    records := make([dynamic]Record, 0, len(springs))
    defer {
        for _, i in records {
            delete(records[i].arrangement)
        }
        delete(records)
    }
    for _, i in springs {
        record := Record{
            arrangement = make([]u8, len(springs[i])),
            damaged_groups = num_damaged[i][:]
        }
        // copy the string contents
        copy(record.arrangement, transmute([]u8)springs[i])
        // line below is apparently accepted/compiling Odin code!??!?!?!
        // append(&records, )
        append(&records, record)
    }

    // move packs of '#' according to num damaged around

    part1_variation_nr_sums := 0
    for rec, i in records
    {
        // ??????#???????????#???? 1,3,1,1,3,1
        // variations 6s vs test_positions 1s
        // num_variations_expected := variations(rec)
        num_variations := test_positions(rec)
        // if num_variations_expected != num_variations {
        //     fmt.println("MISSMATCH:", string(rec.arrangement), "totvar", num_variations, "expected", num_variations_expected)
        // }
        part1_variation_nr_sums += num_variations
    }
    fmt.println("Part1:", part1_variation_nr_sums)

    rec := Record{
        arrangement = transmute([]u8)string("??????#???????????#???????????#????"),
        damaged_groups = {1,3,1,1,3,1,1,3,1}}
        // arrangement = transmute([]u8)string(".??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##."),
        // damaged_groups = {1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3}}
    var := test_positions(rec)
    // num_variations_expected := variations(rec)
    // fmt.println("totvar", var, "expected", num_variations_expected)
    fmt.println("totvar", var)

    // part2
    // split the arrangements into sub-problems
    // we know that e.g.
    // ?###???????? 3,2,1 -> the first # has to be part of the 3er grp
    //                       so split one after it (due to sep)
    // ?#?#?#?#?#?#?#? 1,3,1,6 -> first # -> 1
    //                            second # -> part of 3
    //                            third # -> 1
    //                            rest -> 6
    // store the variations to those sub-problems in a map and look them up
    // 
    cache := make(map[string]int)
    defer delete(cache)
    // rec := Record{
    //     arrangement = transmute([]u8)string(".#"),
    //     damaged_groups = {1}}
    // fmt.println(string(a.arrangement), a.damaged_groups)
    // part2_variation_nr_sums := 0
    // for rec_single, i in records[:1]
    // {
    //     unfolded := unfold(rec_single)
    //     defer {
    //         delete(unfolded.arrangement)
    //         delete(unfolded.damaged_groups)
    //     }
    //     // num_variations_expected := variations(rec)
    //     num_variations := test_positions(unfolded)
    //     part2_variation_nr_sums += num_variations
    // }
    // fmt.println("Part2:", part2_variation_nr_sums)
}

split_subproblems :: proc(rec: Record) -> []Record {
    result := make([dynamic]Record)
    // groups of adjacent ?/#
    // possible_groups := make([dynamic]int)
    in_grp := false
    max_placement := 0
    grp_idx := 0
    for c, i in rec.arrangement {
        switch c {
        case '#':
            if max_placement == 0 {
                // must be part of current grp at grp_idx
            }
        case '?':
            max_placement += 1
        case '.':
        }
    }
    return result[:]
}

unfold :: proc(rec: Record) -> Record {
    // five copies + 4 '?' separators
    arr := make([]u8, len(rec.arrangement) * 5 + 4)
    for i in 0..=4 {
        dest := i * (len(rec.arrangement) + 1)
        copy(arr[dest:], rec.arrangement)
        if i != 4 {
            arr[dest + len(rec.arrangement)] = '?'
        }
    }
    grps := make([]int, len(rec.damaged_groups) * 5)
    for i in 0..=4 {
        copy(grps[i * len(rec.damaged_groups):], rec.damaged_groups)
    }

    return Record{
        arrangement = arr,
        damaged_groups = grps,
    }
}

clone_dyn_array :: proc(dyn: [dynamic]int) -> [dynamic]int {
    dyn_copy := make([dynamic]int, len(dyn), cap(dyn))
    copy(dyn_copy[:], dyn[:])
    return dyn_copy
}

key :: proc (rec: Record, positions: []int) -> string {
    // can't use custom map keys!?!??!
    // -> have to build a string key
    grp_idx_excl := len(positions)
    grp_start := positions[grp_idx_excl - 1]
    grp_len := positions[grp_idx_excl - 1]
    pos_end_excl := grp_start + grp_len
    result := fmt.aprintf("%q%v%v",
        // left-over arrangement/groups
        rec.arrangement[pos_end_excl:],
        rec.damaged_groups[grp_idx_excl:],
        positions)
    fmt.println("made key", result)
    return result
}

add_to_cache :: proc(cache: map[string]int, rec: Record, positions: []int) {
    // add the result for all the intermediates
    #reverse for p, i in positions {
    }
}

fits :: proc(rec: Record, positions: []int) -> bool {
    in_group := false
    grp_size := 0
    grps := make([dynamic]int)
    defer delete(grps)
    for c, i in rec.arrangement {
        // NOTE: this part should not be necessary, since we check for overlaps
        //       below, but just as sanity check
        in_replacement := false
        dot_replaced := false
        for p, grp_idx in positions {
            end_idx_incl := p + rec.damaged_groups[grp_idx] - 1

            // remember that there should be a separator so we can divide the grps
            if i == p - 1 || i == end_idx_incl + 1 {
                dot_replaced = true
            }

            // check if i is inside [i, end_idx_incl]
            if i >= p && i <= end_idx_incl {
                if c == '.' {
                    // part of the damaged group that was placed but no ?/# in
                    // source line
                    return false
                }

                if in_replacement {
                    // two replacements overlap
                    return false
                }
                in_replacement = true

                // check for separation
                if p > 0 && rec.arrangement[p - 1] == '#' {
                    // start not separated
                    return false
                }
                if p > 0 && p < (len(rec.arrangement) - 1) && rec.arrangement[p - 1] == '#' {
                    // end not separated
                    return false
                }

                continue
            }
        }

        if in_replacement || c == '#' {
            in_group = true
            grp_size += 1
        } else if dot_replaced || c == '.' {
            if in_group {
                append(&grps, grp_size)
            }
            in_group = false
            grp_size = 0
        }
    }

    // possible last grp
    if in_group {
        append(&grps, grp_size)
    }

    // fmt.println("fits expected", rec.damaged_groups, "actual", grps)
    if !slice.equal(grps[:], rec.damaged_groups) {
        // groups don't match
        return false
    }

    return true
}

Branch :: struct {
    pos: [dynamic]int
    variations_at_time_of_branching: int
    close: bool
}

// NOTE: this would've been so much easier with a recursive solution,
//       and then you get caching for free (if it's a pure function),
//       since you can just use the input args as keys
//       (we can't do that here with arrangement/grps/positions, since
//        we just count them up)
test_positions :: proc(rec: Record, cache: map[string]int) -> int {
    // contains positions of damaged groups
    stack := make([dynamic][dynamic]int)
    defer delete(stack)
    initial := make([dynamic]int)
    append(&stack, initial)

    variations := 0
    for len(stack) > 0 {
        positions := pop(&stack)
        defer delete(positions)
        current_group_idx := len(positions)

        // look up the state arrangement/groups/positions in the cache
        key := key(rec, positions)
        defer delete(key)
        subvar, ok := cache[key]
        if ok {
            fmt.println("cache hit", key, "->", subvar)
            variations += subvar
            continue
        }

        // part2: need to keep an additional stack or add it to the
        // one we already have, or we could abuse DFS to count variations
        // per branch
        // -> we queue ourselves another time after we queued further branches
        //    when we get our "close" we can calculate the dt in variations
        //    and add it to the cache

        if current_group_idx >= len(rec.damaged_groups) {
            // NOTE: don't need to check if it fits, since the code below only
            //       queus valid positions
            // BUT need to check if the damaged_groups rule is not violated
            // since there might be pre-existing '#' that have to be used,
            // e.g. the below is not valid since the existing # is not used
            //      so there is one two many groups
            // arr ??#?.?#????.? 1, 4
            // var #     ####
            if fits(rec, positions[:]) {
                variations += 1
                // TODO function that adds (for every position in the positions
                //      stack) the nr of variations into the cache
                // fmt.println("arr", string(rec.arrangement))
                // fmt.print("var ")
                // abs_idx := 0
                // for p, gidx in positions {
                //     for _ in abs_idx..=(p - 1) {
                //         fmt.print(" ")
                //     }
                //     abs_idx = p
                //     for _ in 0..=(rec.damaged_groups[gidx] - 1) {
                //         fmt.print("#")
                //         abs_idx += 1
                //     }
                // }
                // fmt.println("")
                // fmt.println(positions)
            }
            // branch finished
            continue
        }

        last_end_excl := 0
        if current_group_idx > 0 {
            // occupied till last position + len
            last_start := positions[current_group_idx - 1]
            // shadowing allowed :/
            // last_end_excl := ...
            // add 1 to account for sep
            last_end_excl = last_start + rec.damaged_groups[current_group_idx - 1] + 1
        }
        
        // occupied from the end until and including this index
        // purely based on the length of following groups
        occupied_till_idx_end_incl := len(rec.arrangement)
        // not last grp
        if current_group_idx != len(rec.damaged_groups) - 1 {
            // fmt.println("grpidx", current_group_idx)
            // fmt.println("lengrps", len(rec.damaged_groups))
            for grp_size in rec.damaged_groups[current_group_idx + 1:] {
                // don't -1, to account for sep that each grp needs
                // TODO: check
                occupied_till_idx_end_incl -= grp_size
            }
        }
        // not enough space left
        if occupied_till_idx_end_incl < 0 {
            continue
        } else if last_end_excl >= occupied_till_idx_end_incl {
            // even if last_end_excl == occupied_till_idx_end_incl
            // would only mean 1 space left
            continue
        }

        // fmt.println("lastend", last_end_excl)
        // fmt.println("occfromend", occupied_till_idx_end_incl)
        start: for _, rel_i in rec.arrangement[last_end_excl:occupied_till_idx_end_incl] {
            idx := last_end_excl + rel_i
            current_start := idx
            current_end_incl := idx + rec.damaged_groups[current_group_idx] - 1

            // beyond end
            if current_end_incl >= len(rec.arrangement) {
                break
            }

            // check start separator
            // either at idx 0 or prev is #/?
            // NOTE: can't get into trouble with prev end, since those always have
            if (current_start != 0) && (rec.arrangement[current_start - 1] == '#') {
                // not separated from the prev
                continue
            }

            for c in rec.arrangement[current_start:current_end_incl + 1] {
                if c == '.' {
                    // can't fit group
                    continue start
                }
            }

            // check end separator
            // either ending at max idx or has a ./? after it
            if current_end_incl != (len(rec.arrangement) - 1) &&
                    (rec.arrangement[current_end_incl + 1] == '#') {
                continue
            }

            to_queque := clone_dyn_array(positions)
            append(&to_queque, current_start)
            append(&stack, to_queque)
        }
    }

    return variations
}

copy_record :: proc(record: Record) -> (result: Record) {
    result.arrangement = make([]u8, len(record.arrangement))
    copy(result.arrangement, record.arrangement)
    result.damaged_groups = record.damaged_groups
    return result
}

// (name: type, ..) -> named return, here with non-zero default value
satisfies_rule :: proc(arrangement: []u8, damaged_groups: []int) -> (satisfies := true) {
    damaged_groups_idx := 0
    num_damaged := 0
    in_group := false
    // fmt.printf("%s\n", arrangement)
    for c in arrangement {
        switch c {
        case '.':
            if in_group {
                if damaged_groups_idx >= len(damaged_groups) {
                    // fmt.println("beyond damaged_groupss")
                    satisfies = false
                    break
                }
                allowed := damaged_groups[damaged_groups_idx]
                // fmt.println("num", num_damaged, "vs allowed", allowed)
                if num_damaged != allowed {
                    // fmt.println("exceeds num allowed")
                    satisfies = false
                    break
                }
                damaged_groups_idx += 1
                num_damaged = 0
                in_group = false
            }
        case '#':
            num_damaged += 1
            in_group = true
        case '?':
            fmt.println("oops")
            os.exit(1)
        }
    }

    if in_group {
        if damaged_groups_idx >= len(damaged_groups) {
            // fmt.println("beyond damaged_groupss")
            satisfies = false
        } else {
            allowed := damaged_groups[damaged_groups_idx]
            // fmt.println("num", num_damaged, "vs allowed", allowed)
            if num_damaged != allowed {
                // fmt.println("exceeds num allowed")
                satisfies = false
            }
            damaged_groups_idx += 1
        }
    }

    if damaged_groups_idx != len(damaged_groups) {
        satisfies = false
    }

    // fmt.println("\tValid:", satisfies, damaged_groups)

    return satisfies
}

variations :: proc(starting_record: Record) -> int {
    stack := make([dynamic]Record)
    defer delete(stack)
    // append a copy so we don't free the caller's record.arrangement
    append(&stack, copy_record(starting_record))

    solutions := 0
    for len(stack) > 0 {
        record := pop(&stack)
        defer delete(record.arrangement)
        // fmt.printf("%s\n", record.arrangement)

        found_unknown := false
        for c, i in record.arrangement {
            if c == '?' {
                record_damaged := copy_record(record)
                record_damaged.arrangement[i] = '#'

                record_working := copy_record(record)
                record_working.arrangement[i] = '.'

                append(&stack, record_damaged)
                append(&stack, record_working)
                found_unknown = true
                break
            }
        }

        if !found_unknown {
            if satisfies_rule(record.arrangement, record.damaged_groups) {
                solutions += 1
            }
        }
    }

    return solutions
}
