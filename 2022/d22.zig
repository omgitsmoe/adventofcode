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

fn switch_sides(current: State3D, new: State3D, step_off: StepOff) !State3D {
    // NOTE: new should contain the new state as if the side was adjacent
    var result = new;

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

    // transform row/col based on amount of turns/the rotation
    // NOTE: can apparently use this per 90deg cw turn (simplified form of 2d matrix rotation):
    // [newX, newY] = [size-1 - newY, newX];  // u/smrq
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
                if (new_col < 0) {
                    // step off "left"

                    // NOTE: !IMPORTANT! don't use state, since it might still be on a different side etc.
                    // look up neighbour in direction .left in the look-up array on the side we're on
                    // then get the appropriate side pointer from the Cube
                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.left)];
                    // print("step off {} dir .left to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .left);
                } else if (new_col >= EDGE_LENGTH) {
                    // step off "right"

                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.right)];
                    // print("step off {} dir .right to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .right);
                } else if (new_row < 0) {
                    // step off "top"

                    const neighbour_kind = test_state.side.neighbours[@enumToInt(StepOff.top)];
                    // print("step off {} dir .top to {}\n", .{ test_state.side.kind, neighbour_kind });
                    test_state.side = get_side_from_kind(cube, neighbour_kind);
                    test_state = try switch_sides(new_state, test_state, .top);
                } else if (new_row >= EDGE_LENGTH) {
                    // step off "bottom"

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
    // (based on a single known neighbour and the neighbours in clockwise order for each side
    //  in neighbours_clockwise)
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
    // use ? to turn ?Side into Side and then take it's address
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
                    // for (x.piece.items) |l| {
                    //     print("{s}\n", .{l});
                    // }
                    for (x.neighbours) |n, di| {
                        print("{}({}): {}\n", .{ @intToEnum(StepOff, di), di, n });
                    }
                    print("\n", .{});
                }
            }
        }
    }

    {
        var state = State3D{ .row = 0, .col = 0, .facing = 1, .side = cube.bottom.? };
        for (movements.items) |mov| {
            const new_pos = try step_3d(cube, state, mov);
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
