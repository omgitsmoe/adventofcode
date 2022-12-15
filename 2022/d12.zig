const std = @import("std");

const Point = struct {
    y: i32,
    x: i32,
};

const Node = struct {
    height: i8,
    path_cost: i32,
    visited: bool,
    pos: Point,
    prev: Point,
};

// no macros in zig
inline fn index(y: i32, x: i32, cols: i32) usize {
    return @intCast(usize, y * cols + x);
}

// call with arena alloc
fn dijkstra(
    allocator: std.mem.Allocator,
    grid: []Node,
    start: Point,
    end: Point,
    cols: i32,
) !usize {
    const rows = @divTrunc(@intCast(i32, grid.len), cols);
    const start_node = &grid[index(start.y, start.x, cols)];
    start_node.visited = true;
    start_node.path_cost = 0;

    // TailQueue uses a doubly linked
    // PriorityQ or PrioriyDeque seems better, but then it would be unfair in comparison
    // to cpp/rust
    const QType = std.TailQueue(Point);
    var q = QType{};
    // need to manage allocations ourselves
    const starting_point = try allocator.create(QType.Node);
    starting_point.data.y = start.y;
    starting_point.data.x = start.x;
    q.prepend(starting_point);

    const neighbours = [_]Point{
        Point{ .y = -1, .x = 0 }, Point{ .y = 1, .x = 0 },
        Point{ .y = 0, .x = 1 },  Point{ .y = 0, .x = -1 },
    };
    while (q.len > 0) {
        // not iter on TailQueue ...
        // find node with lowest path cost
        // ...should really use a prio queue
        var min = q.first orelse unreachable;
        defer q.remove(min);
        // std.debug.print("y {} x {}\n", .{ min.data.y, min.data.x });
        var min_node = &grid[index(min.data.y, min.data.x, cols)];
        var maybe_node: ?*QType.Node = min;
        while (maybe_node) |n| {
            const node = &grid[index(n.data.y, n.data.x, cols)];
            if (node.path_cost < min_node.path_cost) {
                min = n;
                min_node = node;
            }

            // next node in q
            maybe_node = n.next;
        }

        const current = min.data;
        const node = min_node;
        for (neighbours) |n| {
            const next = Point{ .y = current.y + n.y, .x = current.x + n.x };
            if (next.y < 0 or next.y >= rows or next.x < 0 or next.x >= cols) {
                // out of range
                continue;
            }

            const next_node = &grid[index(next.y, next.x, cols)];
            if ((next_node.height - node.height) > 1) {
                // elevation too high
                continue;
            }

            if (node.path_cost + 1 < next_node.path_cost) {
                next_node.path_cost = node.path_cost + 1;
                next_node.prev = current;
            }
            if (!next_node.visited) {
                const to_q = try allocator.create(QType.Node);
                to_q.data.y = next.y;
                to_q.data.x = next.x;
                // std.debug.print("Queueing y {} x {}\n", .{ next.y, next.x });
                q.append(to_q);
                next_node.visited = true;
            }
        }
    }

    const steps = @intCast(usize, grid[index(end.y, end.x, cols)].path_cost);
    return steps;
}

pub fn main() !void {
    // for perf comparisons use: std.heap.c_allocator -> use -lc when building
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("d12.in", .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);
    const trimmed = std.mem.trim(u8, contents, &std.ascii.whitespace);

    var grid = std.ArrayList(Node).init(allocator);
    defer grid.deinit();
    var line_iter = std.mem.split(u8, trimmed, "\n");
    var start = Point{ .y = 0, .x = 0 };
    var end = Point{ .y = 0, .x = 0 };
    var y: i32 = 0;
    var x: i32 = 0;
    while (line_iter.next()) |line| : (y += 1) {
        x = 0;
        for (line) |c| {
            var height: i8 = undefined;
            switch (c) {
                'S' => {
                    height = 0;
                    start.y = y;
                    start.x = x;
                },
                'E' => {
                    height = 'z' - @intCast(i8, 'a');
                    end.y = y;
                    end.x = x;
                },
                else => {
                    height = @intCast(i8, c - 'a');
                },
            }

            try grid.append(
                Node{
                    .height = height,
                    .path_cost = std.math.maxInt(i32),
                    .visited = false,
                    .pos = Point{ .y = y, .x = x },
                    .prev = .{ .y = -1, .x = -1 },
                },
            );

            x += 1;
        }
    }

    const cols = x;

    const t1 = try std.time.Instant.now();

    const grid_copy = try allocator.dupe(Node, grid.items);
    // just to be fair to cpp/rust, otherwise we could just let the os clean up
    defer allocator.free(grid_copy);
    var arena = std.heap.ArenaAllocator.init(allocator);
    {
        const steps1 = try dijkstra(arena.allocator(), grid_copy, start, end, cols);
        // re-use arena memory for part2
        defer {
            arena.state.end_index = 0;
        }

        std.debug.print("Part1: Lowest steps required: {}\n", .{steps1});
    }

    var steps2: usize = std.math.maxInt(usize);
    for (grid.items) |node| {
        if (node.height == 0) {
            // instead of freeing and re-allocating just overwrite with original state
            std.mem.copy(Node, grid_copy, grid.items);
            const steps = try dijkstra(arena.allocator(), grid_copy, node.pos, end, cols);
            if (steps < steps2) {
                steps2 = steps;
            }
        }
    }

    std.debug.print("Part2: Lowest steps required: {}\n", .{steps2});

    // since is in ns -> convert to mili
    const t2 = try std.time.Instant.now();
    const took_ms = t2.since(t1) / std.time.ns_per_ms;
    std.debug.print("Took: {}ms\n", .{took_ms});
    // ~26ms zig ~26ms cpp ~20ms rust
}
