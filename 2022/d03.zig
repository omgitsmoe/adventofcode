const std = @import("std");

// does this work without ptr?
fn mark_items(items: *std.bit_set.StaticBitSet(53), str: []const u8) void {
    for (str) |c| {
        const idx = if (c <= @as(i32, 'Z'))
            c - @as(i32, 'A') + 26
        else
            c - @as(i32, 'a');
        items.set(@intCast(usize, idx));
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d03.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);

    var split_iter = std.mem.split(u8, contents, "\n");
    var point_sum: u32 = 0;
    while (split_iter.next()) |line| {
        if (line.len == 0) continue;

        const half = line.len / 2;
        const compartment1 = line[0..half];
        const compartment2 = line[half..line.len];
        var items1 = std.bit_set.StaticBitSet(53).initEmpty();
        mark_items(&items1, compartment1);
        var items2 = std.bit_set.StaticBitSet(53).initEmpty();
        mark_items(&items2, compartment2);
        // result will be saved in first set
        items1.setIntersection(items2);
        // assumes there's only one DUPLICATE
        point_sum += 1 + @intCast(u32, (items1.findFirstSet() orelse @panic("No active bit")));
    }

    split_iter = std.mem.split(u8, contents, "\n");
    var common_point_sum: u32 = 0;
    while (true) {
        var items1 = std.bit_set.StaticBitSet(53).initEmpty();
        mark_items(&items1, split_iter.next() orelse break);
        var items2 = std.bit_set.StaticBitSet(53).initEmpty();
        mark_items(&items2, split_iter.next() orelse break);
        var items3 = std.bit_set.StaticBitSet(53).initEmpty();
        mark_items(&items3, split_iter.next() orelse break);

        // result will be saved in first set
        items1.setIntersection(items2);
        items1.setIntersection(items3);
        // assumes there's only one DUPLICATE
        common_point_sum += 1 + @intCast(
            u32,
            (items1.findFirstSet() orelse @panic("No active bit")),
        );
    }

    std.debug.print("Part1: Duplicate item prios are {}\n", .{point_sum});
    std.debug.print("Part2: Common items prios are {}\n", .{common_point_sum});
}
