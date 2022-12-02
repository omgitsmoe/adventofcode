const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d01.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);

    var elves = std.ArrayList(i32).init(allocator);

    var split_iter = std.mem.split(u8, contents, "\n");
    var cur_sum: i32 = 0;
    var cur_max: i32 = 0;
    var cur_max_idx: i32 = 0;
    while (split_iter.next()) |line| {
        std.debug.print("{s}\n", .{line});
        if (line.len == 0) {
            if (cur_sum > cur_max) {
                cur_max = cur_sum;
                cur_max_idx = @intCast(i32, elves.items.len);
            }
            try elves.append(cur_sum);
            cur_sum = 0;
        } else {
            const rations: i32 = try std.fmt.parseInt(i32, line, 10);
            cur_sum += rations;
        }
    }

    std.debug.print("Elve {} has {} rations!\n", .{ cur_max_idx, cur_max });

    std.sort.sort(i32, elves.items, {}, comptime std.sort.desc(i32));
    const top_3_sum = blk: {
        var sum: i32 = 0;
        for (elves.items[0..3]) |r| {
            sum += r;
        }

        break :blk sum;
    };

    std.debug.print("The top 3 elves carry {} rations!", .{top_3_sum});
}
