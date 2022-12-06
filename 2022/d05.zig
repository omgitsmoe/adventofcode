const std = @import("std");

const Move = struct {
    num: u8,
    src: u8,
    dest: u8,
};

fn clone(
    src: std.ArrayList(std.ArrayList(u8)),
    allocator: std.mem.Allocator,
) !std.ArrayList(std.ArrayList(u8)) {
    var result = std.ArrayList(std.ArrayList(u8)).init(allocator);
    for (src.items) |*inner| {
        // not actual clone only (shallow) copy -> but works if contained T is POD
        try result.append(try inner.clone());
        // try result.append(std.ArrayList(u8).init(allocator));
        // try result.items[result.items.len - 1].appendSlice(inner.items);
    }

    return result;
}

// NOTE: there is std.mem.reverse which I couldn't find searching for reverse on the stdlib docs :/
fn reverse(comptime T: type, buf: []T) void {
    switch (buf.len) {
        1 => return,
        // would also work with below
        2 => std.mem.swap(T, &buf[0], &buf[1]),
        else => {
            var from_left: usize = 0;
            var from_right: usize = buf.len - 1;
            while (from_left < from_right) : ({
                from_left += 1;
                from_right -= 1;
            }) {
                std.mem.swap(T, &buf[from_left], &buf[from_right]);
            }
        },
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d05.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);

    var stacks = std.ArrayList(std.ArrayList(u8)).init(allocator);
    // tokenize better if you don't care about empty lines
    var split_iter = std.mem.split(u8, contents, "\n");
    outer: while (split_iter.next()) |line| {
        var idx: u32 = 0;
        var col: u32 = 0;
        while (idx < line.len) : ({
            idx += 4;
            col += 1;
        }) {
            switch (line[idx]) {
                '[' => {
                    while (col >= stacks.items.len) {
                        // std.debug.print("Create stack {} col {}\n", .{ stacks.items.len, col });
                        try stacks.append(std.ArrayList(u8).init(allocator));
                    }
                    // std.debug.print("S {} Crate {c}\n", .{ col, line[idx + 1] });
                    try stacks.items[col].append(line[idx + 1]);
                },
                ' ' => {
                    if (line[idx + 1] == '1') {
                        for (stacks.items) |*stack| {
                            reverse(u8, stack.items);
                        }
                        break :outer;
                    }
                },
                else => {},
            }
        }
    }

    var moves = std.ArrayList(Move).init(allocator);
    // continue with same iterator
    while (split_iter.next()) |line| {
        var space_iter = std.mem.split(u8, line, " ");
        var num_buf: [3]u8 = undefined;
        var num_idx: u32 = 0;
        while (space_iter.next()) |word| {
            if (word.len == 0) continue;
            switch (word[0]) {
                '0'...'9' => {
                    num_buf[num_idx % 3] = try std.fmt.parseInt(u8, word, 10);
                    if ((num_idx + 1) % 3 == 0) {
                        try moves.append(Move{
                            .num = num_buf[0],
                            .src = num_buf[1] - 1,
                            .dest = num_buf[2] - 1,
                        });
                    }
                    num_idx += 1;
                },
                else => continue,
            }
        }
    }

    // for (stacks.items) |stack| {
    //     std.debug.print("Stack '", .{});
    //     for (stack.items) |c| {
    //         std.debug.print("{c}", .{c});
    //     }
    //     std.debug.print("'\n", .{});
    // }
    // std.debug.print("{any}\n", .{moves.items});

    {
        // gonna "leak"
        var stack_copy = try clone(stacks, allocator);
        for (moves.items) |move| {
            var idx: u8 = 0;
            while (idx < move.num) : (idx += 1) {
                // std.debug.print("Move {} from {} to {}\n", .{ move.num, move.src, move.dest });
                try stack_copy.items[move.dest].append(stack_copy.items[move.src].pop());
            }
        }

        std.debug.print("Part1 Top stacks: ", .{});
        for (stack_copy.items) |stack| {
            std.debug.print("{c}", .{stack.items[stack.items.len - 1]});
        }
        std.debug.print("\n", .{});
    }
    {
        // gonna "leak"
        var stack_copy = try clone(stacks, allocator);
        for (moves.items) |move| {
            const src = &stack_copy.items[move.src];
            try stack_copy.items[move.dest].appendSlice(src.items[src.items.len - move.num ..]);
            try src.resize(src.items.len - move.num);
        }

        std.debug.print("Part2 Top stacks: ", .{});
        for (stack_copy.items) |stack| {
            std.debug.print("{c}", .{stack.items[stack.items.len - 1]});
        }
        std.debug.print("\n", .{});
    }
}

test "slice reverse" {
    var arr_even = [_]u8{ 0, 1, 2, 3 };
    reverse(u8, &arr_even);

    try std.testing.expectEqual(arr_even, [_]u8{ 3, 2, 1, 0 });

    var arr_uneven = [_]u8{ 0, 1, 2, 3, 4 };
    reverse(u8, &arr_uneven);

    try std.testing.expectEqual(arr_uneven, [_]u8{ 4, 3, 2, 1, 0 });
}
