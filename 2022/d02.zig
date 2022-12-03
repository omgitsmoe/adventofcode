const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d02.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);

    var split_iter = std.mem.split(u8, contents, "\n");
    var points: i32 = 0;
    var points2: i32 = 0;
    while (split_iter.next()) |line| {
        if (line.len == 0) continue;

        const opponent = @intCast(i32, line[0] - 'A');
        const me = @intCast(i32, line[2] - 'X');
        // see cpp for explanation
        const outcome = @mod(me - opponent + 1, 3);

        points += outcome * 3;
        points += me + 1;

        // part2
        const needed_outcome = me;
        const needed_me: i32 = @mod(opponent - 1 + needed_outcome, 3);
        points2 += needed_outcome * 3 + needed_me + 1;
    }

    std.debug.print("Part1: Total points are {}\n", .{points});
    std.debug.print("Part2: Total points are {}\n", .{points2});
}
