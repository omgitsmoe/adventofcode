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
        var idx: u32 = 0;
        while (idx < 53) : (idx += 1) {
            if (items1.isSet(idx)) {
                point_sum += idx + 1;
            }
        }
    }

    split_iter = std.mem.split(u8, contents, "\n");
    var common_point_sum: u32 = 0;
    var rucksacks = [3]std.bit_set.StaticBitSet(53){
        std.bit_set.StaticBitSet(53).initEmpty(),
        std.bit_set.StaticBitSet(53).initEmpty(),
        std.bit_set.StaticBitSet(53).initEmpty(),
    };
    // start at 1 otherwise mod3 is instantly ==0
    var idx: u32 = 1;
    while (split_iter.next()) |line| : (idx += 1) {
        if (line.len == 0) continue;

        const items = &rucksacks[idx % 3];
        mark_items(items, line);

        if (idx % 3 == 0) {
            // result will be saved in first set
            rucksacks[0].setIntersection(rucksacks[1]);
            rucksacks[0].setIntersection(rucksacks[2]);
            var bit_idx: u32 = 0;
            while (bit_idx < 53) : (bit_idx += 1) {
                if (rucksacks[0].isSet(bit_idx)) {
                    common_point_sum += bit_idx + 1;
                }
            }

            // reset bitsets
            rucksacks[0] = std.bit_set.StaticBitSet(53).initEmpty();
            rucksacks[1] = std.bit_set.StaticBitSet(53).initEmpty();
            rucksacks[2] = std.bit_set.StaticBitSet(53).initEmpty();
        }
    }

    std.debug.print("Part1: Duplicate item prios are {}\n", .{point_sum});
    std.debug.print("Part2: Common items prios are {}\n", .{common_point_sum});
}
