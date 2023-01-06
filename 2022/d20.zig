const std = @import("std");

const List = std.TailQueue(i64);

fn move_from_node(list: List, node: *List.Node, idx: i64) !*List.Node {
    if (idx == 0) return node;

    const forwards = idx > 0;
    // decrease amount we have to move by wrapping with modulo first
    // NOTE: !IMPORTANT! since we're moving an element around it's not considered
    // part of the list and should be ignored
    // -> we could either
    // a) remove the item before iterating over the list
    // or
    // b) since we only iterate up to list.len - 1 times as an optimization
    //    we can just decrease the length by one and then do the move
    //    (works since we'd never iterate over ourselves)
    // => use b) with list.len -1
    // (was stuck on this for a while, since it worked fine for the example input)
    // (input that triggers that edge case: [1 5 -2 0] thanks to u/SquireOfFire)
    var to_move = @intCast(usize, try std.math.absInt(idx)) % (list.len - 1);
    var cur = node;
    if (forwards) {
        while (to_move > 0) : (to_move -= 1) {
            if (cur.next) |next| {
                cur = next;
            } else {
                cur = list.first.?;
            }
        }
    } else {
        while (to_move > 0) : (to_move -= 1) {
            if (cur.prev) |prev| {
                cur = prev;
            } else {
                cur = list.last.?;
            }
        }
    }

    return cur;
}

fn print_list(list: List) void {
    var mnext = list.first;
    while (mnext) |next| : (mnext = next.next) {
        std.debug.print("{}, ", .{next.data});
    }
    std.debug.print("\n", .{});
}

// NOTE: resulting list will not have the correct order starting from list.first
// but the order will be correct relative to the node that contains 0
// -> changed, see below will now have "correct" order
fn mix(to_visit: []*List.Node, list: *List) !void {
    for (to_visit) |v| {
        const insert_point = try move_from_node(list.*, v, v.data);

        // in case we reache same pos again
        if (insert_point != v) {
            list.remove(v);

            // v > 0 -> moving forwards so node we're "replacing" gets shifted to the right
            // othterwise to the left
            if (v.data > 0) {
                // manual wrapping at end points
                // NOTE: only matters for the order starting from list.first
                // relative order from the zero node will still be correct without this
                if (insert_point == list.last) {
                    list.insertBefore(list.first.?, v);
                } else {
                    list.insertAfter(insert_point, v);
                }
            } else {
                if (insert_point == list.first) {
                    list.insertAfter(list.last.?, v);
                } else {
                    list.insertBefore(insert_point, v);
                }
            }
        }
        // print_list(list.*);
    }
}

fn save_order(allocator: std.mem.Allocator, list: List) ![]*List.Node {
    var q = std.ArrayList(*List.Node).init(allocator);
    var maybe_next = list.first;
    while (maybe_next) |next| : (maybe_next = next.next) {
        try q.append(next);
    }

    return q.toOwnedSlice();
}

const USE_EXAMPLE = false;

pub fn main() !void {
    // for perf comparisons use: std.heap.c_allocator -> use -lc when building
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    const file = try std.fs.cwd().openFile(
        if (USE_EXAMPLE) "d20.example.in" else "d20.in",
        .{ .mode = .read_only },
    );
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(contents);
    const trimmed = std.mem.trim(u8, contents, &std.ascii.whitespace);
    // const trimmed = "1\n5\n-2\n0"; //std.mem.trim(u8, contents, &std.ascii.whitespace);
    var lines_iter = std.mem.split(u8, trimmed, "\n");
    var list = List{};
    while (lines_iter.next()) |l| {
        if (l.len > 0) {
            const new = try arena_alloc.create(List.Node);
            new.data = try std.fmt.parseInt(i64, l, 10);
            list.append(new);
        }
    }
    // print_list(list);

    // copy the list for part2 and multiply by encryption key
    // large indices won't be a problem for us since move_from_node at most only ever
    // iterates as many times as the list is long - 1
    const ENCRYPTION_KEY: i64 = 811589153;
    var list_copy = List{};
    var mb_next = list.first;
    while (mb_next) |next| : (mb_next = next.next) {
        const new = try arena_alloc.create(List.Node);
        new.data = next.data * ENCRYPTION_KEY;
        list_copy.append(new);
    }
    //

    {
        const order = try save_order(allocator, list);
        defer allocator.free(order);
        try mix(order, &list);
        var zero_node = list.first.?;
        while (zero_node.data != 0) : (zero_node = zero_node.next.?) {}
        const onek = try move_from_node(list, zero_node, 1_000);
        const twok = try move_from_node(list, zero_node, 2_000);
        const threek = try move_from_node(list, zero_node, 3_000);
        std.log.info("1k {}, 2k {}, 3k {}", .{ onek.data, twok.data, threek.data });
        std.log.info("Part1: Sum of 1k-, 2k- and 3k-th value: {}", .{onek.data + twok.data + threek.data});
    }

    {
        // save the original order, which will be used for iteration for all the 10 times we mix
        const order = try save_order(allocator, list_copy);
        defer allocator.free(order);
        var i: u8 = 0;
        // mix 10 times
        while (i < 10) : (i += 1) {
            try mix(order, &list_copy);
        }
        var zero_node = list_copy.first.?;
        while (zero_node.data != 0) : (zero_node = zero_node.next.?) {}
        const onek = try move_from_node(list_copy, zero_node, 1_000);
        const twok = try move_from_node(list_copy, zero_node, 2_000);
        const threek = try move_from_node(list_copy, zero_node, 3_000);
        std.log.info("1k {}, 2k {}, 3k {}", .{ onek.data, twok.data, threek.data });
        std.log.info("Part2: Sum of 1k-, 2k- and 3k-th value: {}", .{onek.data + twok.data + threek.data});
    }
}
