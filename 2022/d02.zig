const std = @import("std");

fn get_option(byte: u8) i32 {
    return switch (byte) {
        'A', 'X' => 1,
        'B', 'Y' => 2,
        'C', 'Z' => 3,
        else => @panic("Unexpected option"),
    };
}

const Outcome = enum(u8) {
    LOSE = 0,
    DRAW = 3,
    WIN = 6,
};
const OUTCOMES = [_]Outcome{ .DRAW, .LOSE, .WIN };

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

        const opponent = get_option(line[0]);
        const me = get_option(line[2]);
        const outcome_idx = @mod(opponent - me, 3);
        const outcome = OUTCOMES[@intCast(usize, outcome_idx)];

        points += @enumToInt(outcome);
        points += me;

        // part2
        // if opponent chooses rock:
        // need to choose same to DRAW; +1 (so paper) to WIN; +2 (so scissors) to LOSE;
        // if opponent chooses paper:
        // need to choose same to DRAW; +1 (so scissors) to WIN; +2 (so rock) to LOSE;
        var needed_outcome: Outcome = undefined;
        var outcome_shift: i32 = undefined;
        switch (line[2]) {
            'X' => {
                // need opponent to WIN
                needed_outcome = .LOSE;
                outcome_shift = 2;
            },
            'Y' => {
                needed_outcome = .DRAW;
                outcome_shift = 0;
            },
            'Z' => {
                // need opponent to LOSE
                needed_outcome = .WIN;
                outcome_shift = 1;
            },
            else => @panic("Unknown input!"),
        }

        const needed_me = 1 + @mod(outcome_shift + opponent - 1, 3);
        points2 += @enumToInt(needed_outcome) + needed_me;
    }

    std.debug.print("Part1: Total points are {}\n", .{points});
    std.debug.print("Part2: Total points are {}\n", .{points2});
}
