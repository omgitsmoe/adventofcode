const std = @import("std");
const print = std.debug.print;

const USE_EXAMPLE = false;
const EDGE_LENGTH: isize = if (USE_EXAMPLE) 4 else 50;

const Move = union(enum) {
    steps: u16,
    turn: i16,
};

const State = struct {
    row: isize,
    col: isize,
    // up 0, right, down, left 3
    facing: u8,
};

fn absolute_to_relative_column(row: isize, col: isize, line_start_end: []const [2]u16) isize {
    const result = col - line_start_end[@intCast(usize, row)][0];
    //print("a->r {} from r{}c{}\n", .{ result, row, col });
    return result;
}

fn in_row_bounds(row: isize, abs_col: isize, line_start_end: []const [2]u16) bool {
    const ri = line_start_end[@intCast(usize, row)];
    if (abs_col >= ri[0] and abs_col <= ri[1]) {
        return true;
    } else {
        return false;
    }
}

fn abs_index_into(grid: []const []const u8, row: isize, abs_col: isize, line_start_end: []const [2]u16) u8 {
    //print("r{} absC{}\n", .{ row, abs_col });
    const rel_col = absolute_to_relative_column(row, abs_col, line_start_end);
    //print("r{} relC{}\n", .{ row, rel_col });
    const row_items = grid[@intCast(usize, row)];
    if (rel_col >= 0 and rel_col < row_items.len) {
        return row_items[@intCast(usize, rel_col)];
    } else {
        return ' ';
    }
}

fn step(
    grid: []const []const u8,
    state: State,
    move: Move,
    line_start_end: []const [2]u16,
) State {
    var new_state = state;
    switch (move) {
        .turn => |t| {
            new_state.facing = @intCast(u8, @mod((new_state.facing + t), 4));
            //print("new facing: {}\n", .{new_state.facing});
        },
        .steps => |steps_num| {
            var steps = steps_num;
            const offset: i16 = switch (new_state.facing) {
                0 => -1,
                1 => 1,
                2 => 1,
                3 => -1,
                else => unreachable,
            };
            switch (new_state.facing) {
                1, 3 => {
                    while (steps > 0) : (steps -= 1) {
                        var new_col = new_state.col + offset;
                        const row_bounds = line_start_end[@intCast(usize, new_state.row)];
                        if (new_col < row_bounds[0]) {
                            new_col = row_bounds[1];
                        } else if (new_col > row_bounds[1]) {
                            new_col = row_bounds[0];
                        }
                        //print("Hor Next r{}c{} facing {}\n", .{ new_state.row, new_col, new_state.facing });

                        if (abs_index_into(grid, new_state.row, new_col, line_start_end) == '#') {
                            // wall -> stop movement
                            break;
                        }

                        new_state.col = new_col;
                        //print("Set c{}\n", .{new_col});
                    }
                },
                0, 2 => {
                    while (steps > 0) : (steps -= 1) {
                        var new_row: isize = new_state.row + offset;
                        //print("Vert Next r{}c{} facing {}\n", .{ new_row, new_state.col, new_state.facing });
                        if (new_row < 0) {
                            new_row = @intCast(isize, line_start_end.len - 1);
                        } else if (new_row >= line_start_end.len) {
                            new_row = 0;
                        }

                        const c = abs_index_into(grid, new_row, new_state.col, line_start_end);
                        if (c == '#') {
                            // wall
                            break;
                        } else if (c == ' ') {
                            // wrap top/bottom
                            // go in the opposite direction till we hit an empty field again
                            var next_new_row = @mod((new_row + offset), @intCast(isize, grid.len));
                            while (true) : (next_new_row = @mod(next_new_row + offset, @intCast(isize, grid.len))) {
                                if (in_row_bounds(next_new_row, new_state.col, line_start_end)) {
                                    break;
                                }
                            }

                            const next_c =
                                abs_index_into(grid, next_new_row, new_state.col, line_start_end);
                            if (next_c == '#') {
                                // check hitting a wall when trying to wrap
                                break;
                            } else {
                                new_row = next_new_row;
                            }
                        }

                        new_state.row = new_row;
                        //print("Set r{}\n", .{new_row});
                    }
                },
                else => unreachable,
            }
        },
    }

    return new_state;
}

const Cube = struct {
    top: ?*Side,
    bottom: ?*Side,
    right: ?*Side,
    left: ?*Side,
    front: ?*Side,
    back: ?*Side,
};

const Side = struct {
    row: isize,
    col: isize,
    piece: std.ArrayList([]const u8),
    kind: Type,
    // order like facing/StepOff
    neighbours: [4]Side.Type,

    const Type = enum {
        none,
        bottom,
        front,
        left,
        right,
        top,
        back,
    };
};

const Pos = struct {
    row: usize,
    col: usize,
};

const State3D = struct {
    row: isize,
    col: isize,
    // up 0, right, down, left 3
    facing: u8,
    side: *const Side,
};

const StepOff = enum {
    top,
    right,
    bottom,
    left,
};

const NeighbourInfo = struct {
    // order: up, right, bottom, left
    // same as StepOff/facing
    // viewed from "outside" as opposed to inside of the cube
    bottom: [4]Side.Type,
    top: [4]Side.Type,
    front: [4]Side.Type,
    back: [4]Side.Type,
    left: [4]Side.Type,
    right: [4]Side.Type,
};

fn switch_sides(current: State3D, new: State3D, step_off: StepOff) !State3D {
    // NOTE: new should contain the new state as if the side was adjacent
    var result = new;
    // // manhattan dist: 1  2   3
    // // turn cw         0 90 180
    // // we can't walk from left to right and vice versa, so we don't have to account for that
    // // negative value since facing+1 is a 90deg cw turn, so -1 is ccw
    // // const turns_cw_per_col_dist: i8 = if (current.side.kind == .left or current.side.kind == .right or
    // //    new.side.kind == .left or new.side.kind == .right) -2 else -1;
    // const dr = try std.math.absInt(current.side.row - new.side.row);
    // const dc = try std.math.absInt(current.side.col - new.side.col);
    // var manhattan_dist = dr + dc;
    // // side we're switching to is left -> ccw turn else clock-wise
    // // we're currently left of the side we're switching to (on the map) / target is right,
    // // so we turn right/cw
    // // else ccw
    // // up+right ccw
    // // up+left cw
    // // up reveresed ^
    // // down+right cw
    // // down+left ccw
    // // const turn_direction: i8 = if (current.side.col < new.side.col) 1 else -1;
    // const turn_direction: i8 = if (current.side.row < new.side.row) blk: {
    //     // target below
    //     break :blk if (current.side.col < new.side.col) 1 else -1;
    // } else blk: {
    //     const turn_dir: i8 = if (current.side.col < new.side.col) 1 else -1;
    //     if (current.side.col == cube.bottom.?.col) {
    //         // target up and same col as the bottom side -> reversed
    //         break :blk turn_dir * -1;
    //     } else {
    //         break :blk turn_dir;
    //     }
    // };
    // // same row or col -> same facing
    // const turns_cw = if (dr == 0 or dc == 0 or manhattan_dist <= 1) 0 else @mod(
    //     (manhattan_dist - 1) * turn_direction,
    //     4,
    // );
    // result.facing = @intCast(u8, @mod((new.facing + turns_cw), 4));
    // // if ang == 90 can use same offset, ang == 180 -> calc offset from middle and go into
    // // opposite direction
    // // -> can't use middle since even side length -> do same from top
    // print("dr{}dc{} turnsCW{}\n", .{ dr, dc, turns_cw });

    // use opposite direction and check on new's neighbour lookup since that's our target
    // + 2 to get opposite direction
    const step_off_idx: u8 = (@intCast(u8, @enumToInt(step_off)) + 2) % 4;
    var turns_cw: u8 = 0;
    // find out how many turns we need to reach correct direction to step off to the
    // new side
    // (need to check new side and use the opposite direction ((stepoff + 2) mod 4), since
    //  we already used current.side.neighbours to determine the side based on step_off,
    //  so the turns would always be 0)
    while (new.side.neighbours[@intCast(usize, turns_cw + step_off_idx) % 4] !=
        current.side.kind) : (turns_cw = (turns_cw + 1) % 4)
    {}
    result.facing = @intCast(u8, @mod((new.facing + turns_cw), 4));
    // print("turnsCW{}\n", .{turns_cw});
    switch (turns_cw) {
        0 => {
            switch (step_off) {
                .left => {
                    result.row = new.row;
                    result.col = EDGE_LENGTH - 1;
                },
                .right => {
                    result.row = new.row;
                    result.col = 0;
                },
                .top => {
                    result.row = EDGE_LENGTH - 1;
                    result.col = new.col;
                },
                .bottom => {
                    result.row = 0;
                    result.col = new.col;
                },
            }
        },
        1 => {
            // 90deg cw
            switch (step_off) {
                .right => {
                    result.row = 0;
                    result.col = EDGE_LENGTH - 1 - new.row;
                },
                .left => {
                    result.row = EDGE_LENGTH - 1;
                    result.col = EDGE_LENGTH - 1 - new.row;
                    // result.row = new.col;
                    // result.col = EDGE_LENGTH - 1;
                },
                .top => {
                    result.row = new.col;
                    result.col = 0;
                },
                .bottom => {
                    result.row = new.col;
                    result.col = EDGE_LENGTH - 1;
                },
            }
        },
        2 => {
            switch (step_off) {
                .right => {
                    // same offset from other side
                    result.row = EDGE_LENGTH - 1 - new.row;
                    result.col = EDGE_LENGTH - 1;
                },
                .left => {
                    result.row = EDGE_LENGTH - 1 - new.row;
                    result.col = 0;
                },
                .top => {
                    result.row = 0;
                    // same offset from other side
                    result.col = EDGE_LENGTH - 1 - new.col;
                },
                .bottom => {
                    result.row = EDGE_LENGTH - 1;
                    result.col = EDGE_LENGTH - 1 - new.col;
                },
            }
        },
        3 => {
            switch (step_off) {
                .right => {
                    result.row = EDGE_LENGTH - 1;
                    result.col = new.row;
                },
                .left => {
                    result.row = 0;
                    result.col = new.row;
                },
                .top => {
                    result.row = EDGE_LENGTH - 1 - new.col;
                    result.col = EDGE_LENGTH - 1;
                },
                .bottom => {
                    result.row = EDGE_LENGTH - 1 - new.col;
                    result.col = 0;
                },
            }
        },
        else => unreachable,
    }
    return result;
}

// for getting neighbour, based on the current side we're on, the direction we're stepping off of it
// and the neighbour_info to look up the appropriate neighbour
// NOTE: fails for looking up some edge cases
fn get_next_side(cube: Cube, current_side: *const Side, dir: StepOff, neighbour_info: NeighbourInfo) !*Side {
    // const dr = try std.math.absInt(current_side.row - cube.bottom.?.row);
    const dc = try std.math.absInt(current_side.col - cube.bottom.?.col);
    // const manhattan_dist = dr + dc;
    // // side we're switching to is left -> ccw turn else clock-wise
    // // we're currently left of the side we're switching to (on the map), so we turn right/cw
    // // else ccw
    // const turn_direction: i8 = if (current_side.col < cube.bottom.?.col) 1 else -1;
    // // same row or col -> same facing
    // // turns_cw
    // const next_side_idx = if (dr == 0 or dc == 0 or manhattan_dist <= 1) @enumToInt(dir) else @intCast(usize, @mod(
    //     (manhattan_dist) * turn_direction + @enumToInt(dir),
    //     4,
    // ));
    // 90 cw per +col, 90ccw per -col (mb 180 per row?)
    const turn_direction: i8 = if (current_side.col < cube.bottom.?.col) -1 else 1;
    // const turns_cw = if (dr == 0 or dc == 0) @enumToInt(dir) else @mod(dc * turn_direction + @enumToInt(dir), 4);
    const turns_cw = @mod(dc * turn_direction + @enumToInt(dir), 4);
    // +90cw per row
    // const turns_cw = @mod(dr + @enumToInt(dir), 4);
    const next_side_idx = @intCast(usize, turns_cw);
    // print("step off side {} in dir {} ({}) -> next side idx {}\n", .{ current_side.kind, dir, @enumToInt(dir), next_side_idx });
    const next_side_kind = switch (current_side.kind) {
        .bottom => neighbour_info.bottom[next_side_idx],
        .top => neighbour_info.top[next_side_idx],
        .front => neighbour_info.front[next_side_idx],
        .back => neighbour_info.back[next_side_idx],
        .left => neighbour_info.left[next_side_idx],
        .right => neighbour_info.right[next_side_idx],
        .none => unreachable,
    };
    // print("{any}\n", .{neighbour_info});
    // print("next side kind {}\n", .{next_side_kind});
    const next_side = switch (next_side_kind) {
        .bottom => cube.bottom.?,
        .top => cube.top.?,
        .front => cube.front.?,
        .back => cube.back.?,
        .left => cube.left.?,
        .right => cube.right.?,
        .none => unreachable,
    };

    return next_side;
}

fn get_side_from_kind(cube: Cube, kind: Side.Type) *Side {
    return switch (kind) {
        .none => unreachable,
        .top => cube.top.?,
        .bottom => cube.bottom.?,
        .front => cube.front.?,
        .back => cube.back.?,
        .left => cube.left.?,
        .right => cube.right.?,
    };
}

fn step_3d(
    cube: Cube,
    state: State3D,
    move: Move,
    // neighbour_info: NeighbourInfo,
) !State3D {
    var new_state = state;
    switch (move) {
        .turn => |t| {
            new_state.facing = @intCast(u8, @mod((new_state.facing + t), 4));
            // print("t {} new facing: {}\n", .{ t, new_state.facing });
        },
        .steps => |steps_num| {
            var steps = steps_num;
            while (steps > 0) : (steps -= 1) {
                // we can switch facing when switching sides so this needs to be inside the loop
                const offset: [2]isize = switch (new_state.facing) {
                    // up
                    0 => .{ -1, 0 },
                    // right
                    1 => .{ 0, 1 },
                    // down
                    2 => .{ 1, 0 },
                    // left
                    3 => .{ 0, -1 },
                    else => unreachable,
                };
                var test_state = new_state;
                const new_row = new_state.row + offset[0];
                const new_col = new_state.col + offset[1];
                // switch sides
                // TODO save neighbours when reading in sides, so know which side to go
                // to, while respecting the actual rotation of the side piece
                if (new_col < 0) {
                    // step off "left"
                    // from "cube" perspective
                    // switch (state.side.kind) {
                    //     .bottom, .front, .top => test_state.side = cube.left.?,
                    //     .back => test_state.side = cube.right.?,
                    //     .left => test_state.side = cube.back.?,
                    //     // wrong for example, needs to be top
                    //     // but can also be front in other cases
                    //     .right => test_state.side = cube.top.?,
                    //     .none => unreachable,
                    // }

                    // NOTE: !IMPORTANT! don't use state, since it might still be on a different side etc.
                    // test_state = try switch_sides(new_state, test_state, .left);
                    // test_state.side = try get_next_side(cube, test_state.side, .left, neighbour_info);
                    // look up neighbour in direction .left in the look-up array on the side we're on
                    // then get the appropriate side pointer from the Cube
                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.left)];
                    // print("step off {} dir .left to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .left);
                } else if (new_col >= EDGE_LENGTH) {
                    // step off "right"
                    // switch (state.side.kind) {
                    //     .bottom, .front, .top => test_state.side = cube.right.?,
                    //     .back => test_state.side = cube.left.?,
                    //     .left => test_state.side = cube.front.?,
                    //     // was back
                    //     .right => test_state.side = cube.bottom.?,
                    //     .none => unreachable,
                    // }

                    // test_state.side = try get_next_side(cube, test_state.side, .right, neighbour_info);
                    // look up neighbour in direction .right in the look-up array on the side we're on
                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.right)];
                    // print("step off {} dir .right to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .right);
                } else if (new_row < 0) {
                    // step off "top"
                    // switch (state.side.kind) {
                    //     .bottom => test_state.side = cube.back.?,
                    //     // was top
                    //     .front => test_state.side = cube.bottom.?,
                    //     // was back
                    //     .top => test_state.side = cube.front.?,
                    //     // was top
                    //     .back => test_state.side = cube.bottom.?,
                    //     // was top
                    //     .left => test_state.side = cube.bottom.?,
                    //     // was top; might also be bottom?? TODO
                    //     .right => test_state.side = cube.front.?,
                    //     .none => unreachable,
                    // }

                    // test_state.side = try get_next_side(cube, test_state.side, .top, neighbour_info);
                    // look up neighbour in direction .top in the look-up array on the side we're on
                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.top)];
                    // print("step off {} dir .top to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .top);
                } else if (new_row >= EDGE_LENGTH) {
                    // step off "bottom"
                    // switch (state.side.kind) {
                    //     .bottom => test_state.side = cube.front.?,
                    //     .front => test_state.side = cube.bottom.?,
                    //     .top => test_state.side = cube.front.?,
                    //     .back => test_state.side = cube.bottom.?,
                    //     // was bottom
                    //     .left, .right => test_state.side = cube.back.?,
                    //     .none => unreachable,
                    // }

                    // test_state.side = try get_next_side(cube, test_state.side, .bottom, neighbour_info);
                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.bottom)];
                    // print("step off {} dir .bottom to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .bottom);
                } else {
                    test_state.row = new_row;
                    test_state.col = new_col;
                }

                if (test_state.side.piece.items[
                    @intCast(usize, test_state.row)
                ][
                    @intCast(usize, test_state.col)
                ] == '#') {
                    // print("stop at wall r{}c{} side: {} facing: {}\n", .{
                    //     test_state.row,
                    //     test_state.col,
                    //     test_state.side.kind,
                    //     test_state.facing,
                    // });
                    // print("=> remains r{}c{} side: {} facing: {}\n", .{
                    //     new_state.row,
                    //     new_state.col,
                    //     new_state.side.kind,
                    //     new_state.facing,
                    // });
                    break;
                }

                new_state = test_state;
                // print("stepping to r{}c{} side: {} facing: {}\n", .{
                //     test_state.row,
                //     test_state.col,
                //     test_state.side.kind,
                //     test_state.facing,
                // });
            }
        },
    }

    return new_state;
}

fn set_neighbours(
    neighbours_clockwise: [6][4]Side.Type,
    // current side
    side: *Side,
    // direction of neighbour with type neighbour_kind
    dir: StepOff,
    // type of neighbour
    neighbour_kind: Side.Type,
) void {
    // set neighbours based on the direction we're setting neighbour_kind from
    // index into the clockwise neighbours
    const dir_idx = @enumToInt(dir);
    // correct neighbour lookup array (for the current side)
    const neighbours_lookup = &neighbours_clockwise[@enumToInt(side.kind) - 1];
    // position of neighbour_kind in the clockwise "sorted" neighbours array
    const face_idx = std.mem.indexOfScalar(
        Side.Type,
        neighbours_lookup,
        neighbour_kind,
    ) orelse unreachable;

    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        // the current direction we're assigning in the neighbour lookup on Side
        const current_direction = (dir_idx + i) % 4;
        // current neighbour idx to look up the "shifted"/"rotated" neighbour in the clockwise lookup array
        const current_neighbour_idx = (face_idx + i) % 4;
        const current_neighbour_kind = neighbours_lookup[current_neighbour_idx];
        // set "rotated" neighbour on current side
        side.neighbours[current_direction] = current_neighbour_kind;
    }
}

pub fn main() !void {
    // for perf comparisons use: std.heap.c_allocator -> use -lc when building
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();
    _ = arena_alloc;

    const file = try std.fs.cwd().openFile(
        // "d22.test.in",
        if (USE_EXAMPLE) "d22.example.in" else "d22.in",
        .{ .mode = .read_only },
    );
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);
    var iter = std.mem.split(u8, contents, "\n");

    const RowType = []const u8;
    var grid = std.ArrayList(RowType).init(allocator);
    var movements_str: []const u8 = undefined;
    var movements = std.ArrayList(Move).init(allocator);
    defer movements.deinit();
    var line_start_end = std.ArrayList([2]u16).init(allocator);
    defer line_start_end.deinit();
    var min_width: usize = std.math.maxInt(usize);
    {
        var line_idx: usize = 0;
        while (iter.next()) |line| : (line_idx += 1) {
            if (line.len == 0) {
                movements_str = iter.next().?;
                break;
            } else {
                var line_start: usize = undefined;
                var in_tunnel = false;
                var last_non_space: usize = undefined;
                for (line) |b, col| {
                    if (b != ' ') {
                        if (!in_tunnel) {
                            line_start = col;
                            last_non_space = col;
                            in_tunnel = true;
                        } else {
                            last_non_space = col;
                        }
                    }
                }

                const width = last_non_space - line_start;
                if (width < min_width) min_width = width;

                try line_start_end.append([2]u16{ @intCast(u16, line_start), @intCast(u16, last_non_space) });
                try grid.append(line[line_start .. last_non_space + 1]);
            }
        }

        var last_alpha_idx: isize = -1;
        for (movements_str) |b, i| {
            if (b == 'R' or b == 'L' or i == (movements_str.len - 1)) {
                const steps: u16 =
                    try std.fmt.parseInt(
                    u16,
                    movements_str[@intCast(usize, last_alpha_idx + 1)..if (i == (movements_str.len - 1)) i + 1 else i],
                    10,
                );
                try movements.append(.{ .steps = steps });
                if (i != (movements_str.len - 1)) {
                    // up, right, down, left => +1 clockwise; -1 counterclockwise
                    try movements.append(.{ .turn = if (b == 'R') 1 else -1 });
                }
                // //print("steps: {}, turn: {c}\n", .{ steps, b });
                last_alpha_idx = @intCast(isize, i);
            }
        }
    }

    //print("{any}\n", .{movements.items});
    //print("{any}\n", .{line_start_end.items});

    // //print("grid: XXX{s}XXX\n", .{grid});
    // //print("movements: {s}\n", .{movements});
    {
        var state = State{ .row = 0, .col = line_start_end.items[0][0], .facing = 1 };
        for (movements.items) |mov| {
            const new_pos = step(grid.items, state, mov, line_start_end.items);
            state = new_pos;
        }

        //print("{any}\n", .{state});
        const converted_facing: isize = switch (state.facing) {
            0 => 3,
            1 => 0,
            2 => 1,
            3 => 2,
            else => unreachable,
        };
        const part1 = (state.row + 1) * 1_000 + (state.col + 1) * 4 + converted_facing;
        print("Part1: Password is {}\n", .{part1});
    }

    // split up the map into foldable pieces of edge length EDGE_LENGTH
    // var sides = std.ArrayList(Side).init(allocator);
    var sides = [_][4]?Side{[_]?Side{null} ** 4} ** 4;
    defer {
        for (sides) |r| {
            for (r) |c| {
                if (c) |s| {
                    s.piece.deinit();
                }
            }
        }
    }
    var first: ?Pos = null;
    for (grid.items) |row, row_idx| {
        const pieces: usize = @divTrunc(row.len, EDGE_LENGTH);
        const piece_row: usize = @divTrunc(row_idx, EDGE_LENGTH);
        var i: usize = 0;
        while (i < pieces) : (i += 1) {
            const piece_col = @intCast(usize, @divTrunc(
                line_start_end.items[row_idx][0] + @intCast(isize, i) * EDGE_LENGTH,
                EDGE_LENGTH,
            ));
            const side = &sides[piece_row][piece_col];
            if (side.* == null) {
                side.* = Side{
                    .row = @intCast(isize, piece_row),
                    .col = @intCast(isize, piece_col),
                    .piece = std.ArrayList([]const u8).init(allocator),
                    .kind = .none,
                    .neighbours = [_]Side.Type{.none} ** 4,
                };
                if (first == null) first = .{
                    .row = piece_row,
                    .col = piece_col,
                };
            }
            // wtf is that syntax
            try side.*.?.piece.append(
                row[i * EDGE_LENGTH .. row.len - (pieces - i - 1) * EDGE_LENGTH],
            );
        }
    }

    // for (sides) |r| {
    //     for (r) |s| {
    //         if (s != null) {
    //             print("#", .{});
    //         } else {
    //             print(".", .{});
    //         }
    //     }
    //     print("\n", .{});
    // }
    // for (sides) |r| {
    //     for (r) |s| {
    //         if (s) |x| {
    //             print("r{}c{}\n", .{ x.row, x.col });
    //             for (x.piece.items) |l| {
    //                 print("{s}\n", .{l});
    //             }
    //         }
    //     }
    // }

    const neighbour_faces_clockwise = [6][4]Side.Type{
        // same as facing/StepOff
        // up, right, below, left
        // bottom (same as Side.Type - 1)
        [4]Side.Type{ .back, .right, .front, .left },
        // front
        [4]Side.Type{ .bottom, .right, .top, .left },
        // left
        [4]Side.Type{ .front, .top, .back, .bottom },
        // right, reverse of left
        [4]Side.Type{ .bottom, .back, .top, .front },
        // top, reverse of bottom
        [4]Side.Type{ .left, .front, .right, .back },
        // back, reverse of front
        [4]Side.Type{ .left, .top, .right, .bottom },
    };

    // start with the top-left piece as the bottom and then figure out the rest
    // when folding the sides into a cube
    var cube: Cube = std.mem.zeroes(Cube);
    // use ? to turon ?Side into Side and then take it's address
    cube.bottom = &(sides[first.?.row][first.?.col]).?;
    cube.bottom.?.kind = .bottom;
    {
        var found_pieces: u8 = 1;
        var q = std.ArrayList(*Side).init(allocator);
        defer q.deinit();
        try q.append(cube.bottom.?);
        const neighbours = [_][2]i32{ .{ 1, 0 }, .{ -1, 0 }, .{ 0, 1 }, .{ 0, -1 } };

        var seen = [_][4]bool{[_]bool{false} ** 4} ** 4;
        while (found_pieces < 6 and q.items.len > 0) {
            const cur = q.pop();
            seen[@intCast(usize, cur.row)][@intCast(usize, cur.col)] = true;

            for (neighbours) |n, ni| {
                const ncol = cur.col + n[0];
                const nrow = cur.row + n[1];

                // can have max 4 pieces in one row/col, otherwise it can't be folded into a cube
                if (nrow < 0 or nrow >= 4 or ncol < 0 or ncol >= 4) continue;
                if (seen[@intCast(usize, nrow)][@intCast(usize, ncol)]) continue;

                if (sides[@intCast(usize, nrow)][@intCast(usize, ncol)]) |*s| {
                    const nkind: Side.Type = switch (ni) {
                        // right
                        0 => switch (cur.kind) {
                            .bottom, .top, .front, .back => blk: {
                                break :blk .right;
                            },
                            .left, .right => blk: {
                                // we know .bottom != null since that's our starting side
                                // can be front/back/top (bottom is the first)
                                // if we already know the other two -> return the other
                                // else re-queue
                                // TODO are there situations where we can infinite loop here?
                                if (cube.front != null) {
                                    if (cube.back != null) {
                                        break :blk .top;
                                    } else if (cube.top != null) {
                                        break :blk .back;
                                    } else {
                                        // re-queue
                                        try q.append(s);
                                        break :blk .none;
                                    }
                                } else if (cube.back != null) {
                                    if (cube.top != null) {
                                        break :blk .front;
                                    } else {
                                        // re-queue
                                        try q.append(s);
                                        break :blk .none;
                                    }
                                } else {
                                    // re-queue
                                    try q.append(s);
                                    break :blk .none;
                                }
                            },
                            .none => unreachable,
                        },
                        // left
                        1 => switch (cur.kind) {
                            .bottom, .top, .front, .back => blk: {
                                break :blk .left;
                            },
                            .left, .right => blk: {
                                // we know .bottom != null since that's our starting side
                                if (cube.front != null) {
                                    if (cube.back != null) {
                                        break :blk .top;
                                    } else if (cube.top != null) {
                                        break :blk .back;
                                    } else {
                                        // re-queue
                                        try q.append(s);
                                        break :blk .none;
                                    }
                                } else if (cube.back != null) {
                                    if (cube.top != null) {
                                        break :blk .front;
                                    } else {
                                        // re-queue
                                        try q.append(s);
                                        break :blk .none;
                                    }
                                } else {
                                    // re-queue
                                    try q.append(s);
                                    break :blk .none;
                                }
                            },
                            .none => unreachable,
                        },
                        // below
                        2 => switch (cur.kind) {
                            .bottom => blk: {
                                break :blk .front;
                            },
                            .top => blk: {
                                break :blk .back;
                            },
                            .front => blk: {
                                break :blk .top;
                            },
                            .back => blk: {
                                break :blk .bottom;
                            },
                            .left, .right => blk: {
                                // we know .bottom != null since that's our starting side
                                if (cube.front != null) {
                                    if (cube.back != null) {
                                        break :blk .top;
                                    } else if (cube.top != null) {
                                        break :blk .back;
                                    } else {
                                        // re-queue
                                        try q.append(s);
                                        break :blk .none;
                                    }
                                } else if (cube.back != null) {
                                    if (cube.top != null) {
                                        break :blk .front;
                                    } else {
                                        // re-queue
                                        try q.append(s);
                                        break :blk .none;
                                    }
                                } else {
                                    // re-queue
                                    try q.append(s);
                                    break :blk .none;
                                }
                            },
                            .none => unreachable,
                        },
                        // above
                        // TODO do we need to look above?
                        3 => blk: {
                            break :blk .none;
                        },
                        else => unreachable,
                    };

                    switch (nkind) {
                        .none => continue,
                        .bottom => unreachable,
                        .top => {
                            if (cube.top == null) {
                                cube.top = s;
                                s.kind = .top;
                                try q.append(s);
                                found_pieces += 1;
                                print("found top from {}: r{}c{}\n", .{ cur.kind, s.row, s.col });
                            }
                        },
                        .front => {
                            if (cube.front == null) {
                                cube.front = s;
                                s.kind = .front;
                                try q.append(s);
                                found_pieces += 1;
                                print("found front from {}: r{}c{}\n", .{ cur.kind, s.row, s.col });
                            }
                        },
                        .back => {
                            if (cube.back == null) {
                                cube.back = s;
                                s.kind = .back;
                                try q.append(s);
                                found_pieces += 1;
                                print("found back from {}: r{}c{}\n", .{ cur.kind, s.row, s.col });
                            }
                        },
                        .left => {
                            if (cube.left == null) {
                                cube.left = s;
                                s.kind = .left;
                                try q.append(s);
                                found_pieces += 1;
                                print("found left from {}: r{}c{}\n", .{ cur.kind, s.row, s.col });
                            }
                        },
                        .right => {
                            if (cube.right == null) {
                                cube.right = s;
                                s.kind = .right;
                                try q.append(s);
                                found_pieces += 1;
                                print("found right from {}: r{}c{}\n", .{ cur.kind, s.row, s.col });
                            }
                        },
                    }

                    if (cur.kind != .none and s.kind != .none) {
                        // set neighbours on s based on the single cur.kind and the direction
                        // the neighbour is in
                        // the rest can be inferred by searching for the offset into a
                        // array of neighbours in clockwise order
                        // and then just inserting starting from that offset

                        // NOTE: could use + 2 mod 4 if we used neighbours offsets with a
                        // clock wise or ccw order
                        const opposite_direction: StepOff = switch (ni) {
                            // right
                            0 => .left,
                            // left
                            1 => .right,
                            // below
                            2 => .top,
                            // above
                            3 => .bottom,
                            else => unreachable,
                        };
                        // use opposite direction, since we're setting our cur.kind as neighbour
                        // of s (the "next" side)
                        set_neighbours(neighbour_faces_clockwise, s, opposite_direction, cur.kind);
                    }
                }
            }
        }

        // special case to assign neighbours for bottom
        const front = cube.front.?;
        const bottom_direction_from_front = std.mem.indexOfScalar(
            Side.Type,
            &front.neighbours,
            .bottom,
        ) orelse unreachable;
        // + 2 and then mod 4 to get opposite direction (assuming StepOff is in cw or ccw order)
        const opposite_direction = @intToEnum(StepOff, (bottom_direction_from_front + 2) % 4);
        // use opposite direction, since we're setting our cur side on the neighbour
        set_neighbours(neighbour_faces_clockwise, cube.bottom.?, opposite_direction, front.kind);

        print("found {} sides\n", .{found_pieces});
        for (sides) |r| {
            for (r) |s| {
                if (s) |x| {
                    const p: u8 = switch (x.kind) {
                        .none => '?',
                        .bottom => 'S',
                        .back => 'b',
                        .front => 'f',
                        .top => 't',
                        .left => 'l',
                        .right => 'r',
                    };
                    print("{c}", .{p});
                } else {
                    print(".", .{});
                }
            }
            print("\n", .{});
        }
        for (sides) |r| {
            for (r) |s| {
                if (s) |x| {
                    print("{}:\n", .{x.kind});
                    for (x.piece.items) |l| {
                        print("{s}\n", .{l});
                    }
                    for (x.neighbours) |n, di| {
                        print("{}({}): {}\n", .{ @intToEnum(StepOff, di), di, n });
                    }
                    print("\n", .{});
                }
            }
        }
    }

    {
        // const neighbour_info = NeighbourInfo{
        //     // order: up, right, bottom, left
        //     // viewed from "outside" as opposed to inside of the cube
        //     // from bottom
        //     .bottom = [4]Side.Type{ .back, .right, .front, .left },
        //     // from top
        //     // bottom with map up = cube up
        //     // TODO .top = [4]Side.Type{ .back, .left, .front, .right },
        //     // top with map up = cube up
        //     .top = [4]Side.Type{ .front, .right, .back, .left },
        //     // from the current side with bottom on bottom
        //     // was: .front = [4]Side.Type{ .bottom, .right, .top, .left },
        //     .front = [4]Side.Type{ .top, .right, .bottom, .left },
        //     // viewed from front with bottom on bottom
        //     // .back = [4]Side.Type{ .top, .left, .bottom, .right },
        //     // viewed from back
        //     .back = [4]Side.Type{ .top, .right, .bottom, .left },
        //     .left = [4]Side.Type{ .top, .back, .bottom, .front },
        //     .right = [4]Side.Type{ .top, .front, .bottom, .back },
        // };

        // viewed from current side where map up = current side up
        // whereas version above bottom would always be at the bottom
        // const neighbour_info = NeighbourInfo{
        //     // order: up, right, bottom, left
        //     .bottom = [4]Side.Type{ .back, .right, .front, .left },
        //     .top = [4]Side.Type{ .front, .right, .back, .left },
        //     .front = [4]Side.Type{ .bottom, .right, .top, .left },
        //     // TODO switch l/r?
        //     .back = [4]Side.Type{ .top, .left, .bottom, .right },
        //     // .back = [4]Side.Type{ .top, .right, .bottom, .left },
        //     .left = [4]Side.Type{ .top, .front, .bottom, .back },
        //     .right = [4]Side.Type{ .top, .back, .bottom, .front },
        // };
        //
        // const test_cube = Cube{
        //     .bottom = &(sides[0][2].?),
        //     .front = &(sides[1][2].?),
        //     .top = &(sides[2][2].?),
        //     .right = &(sides[2][3].?),
        //     .left = &(sides[1][1].?),
        //     .back = &(sides[1][0].?),
        // };
        var state = State3D{ .row = 0, .col = 0, .facing = 1, .side = cube.bottom.? };
        for (movements.items) |mov| {
            const new_pos = try step_3d(cube, state, mov); //, neighbour_info);
            // const new_pos = try step_3d(test_cube, state, mov);
            state = new_pos;
        }

        //print("{any}\n", .{state});
        const converted_facing: isize = switch (state.facing) {
            0 => 3,
            1 => 0,
            2 => 1,
            3 => 2,
            else => unreachable,
        };
        const abs_row = state.row + state.side.row * EDGE_LENGTH;
        const abs_col = state.col + state.side.col * EDGE_LENGTH;
        print("Final facing {} row {}+1 col {}+1\n", .{ converted_facing, abs_row, abs_col });
        const part2 = (abs_row + 1) * 1_000 + (abs_col + 1) * 4 + converted_facing;
        print("Part2: Password is {}\n", .{part2});
    }
}
