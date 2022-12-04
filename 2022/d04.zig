const std = @import("std");

const Range = struct {
    start: i32,
    end_inclusive: i32,

    pub fn from_bytes(bytes: []const u8) !@This() {
        const dash_idx = std.mem.indexOfScalar(u8, bytes, '-') orelse @panic("no dash");
        return Range{
            .start = try std.fmt.parseInt(i32, bytes[0..dash_idx], 10),
            .end_inclusive = try std.fmt.parseInt(i32, bytes[dash_idx + 1 ..], 10),
        };
    }

    pub fn contains(self: @This(), other: @This()) bool {
        var ret = false;
        if (self.start <= other.start and
            self.start <= other.end_inclusive and
            self.end_inclusive >= other.end_inclusive)
        {
            ret = true;
        }

        return ret;
    }

    pub fn overlaps(self: @This(), other: @This()) bool {
        var ret = true;
        if (self.start > other.end_inclusive) {
            // starts after other
            ret = false;
        } else if (self.start < other.start and self.end_inclusive < other.start) {
            // start+ends before other
            ret = false;
        }

        return ret;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d04.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);

    var split_iter = std.mem.split(u8, contents, "\n");
    var fully_contained: u32 = 0;
    var overlapping: u32 = 0;
    while (split_iter.next()) |line| {
        if (line.len == 0) continue;

        const comma_idx = std.mem.indexOfScalar(u8, line, ',') orelse @panic("no comma");
        const r1_str = line[0..comma_idx];
        const r2_str = line[comma_idx + 1 ..];
        const range1 = try Range.from_bytes(r1_str);
        const range2 = try Range.from_bytes(r2_str);

        if (range1.contains(range2) or range2.contains(range1)) {
            fully_contained += 1;
        }
        if (range1.overlaps(range2)) {
            overlapping += 1;
        }
    }
    std.debug.print("Part1: Fully contained ranges {}\n", .{fully_contained});
    std.debug.print("Part2: Overlaps {}\n", .{overlapping});
}
