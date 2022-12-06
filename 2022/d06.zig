const std = @import("std");

fn find_unique_window(str: []const u8, window_size: u8) void {
    var idx: usize = 0;
    while (idx < str.len - window_size) : (idx += 1) {
        const window = str[idx .. idx + window_size];
        var unique = true;
        outer: for (window) |c, i| {
            if (i == window.len - 1) break;
            for (window[i + 1 ..]) |d| {
                if (c == d) {
                    unique = false;
                    break :outer;
                }
            }
        }

        if (unique) {
            std.debug.print("Marker pos {}\n", .{idx + window_size});
            break;
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d06.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);
    const trimmed = std.mem.trim(u8, contents, &std.ascii.whitespace);
    find_unique_window(trimmed, 4);
    find_unique_window(trimmed, 14);
}
