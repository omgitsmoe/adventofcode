// See https://aka.ms/new-console-template for more information
string[] lines = File.ReadAllLines("../d09.in");
List<Move> moves = new();
foreach (string l in lines) {
    string[] dir_times = l.Split();
    Move m = new();
    switch (dir_times[0]) {
        case "U":
            m.dy = -1;
            break;
        case "D":
            m.dy = 1;
            break;
        case "R":
            m.dx = 1;
            break;
        case "L":
            m.dx = -1;
            break;
    }

    m.num_steps = int.Parse(dir_times[1]);
    moves.Add(m);
}

{
    Point head = new();
    Point tail = new();
    HashSet<Point> visited = new();
    foreach (Move m in moves) {
        for (int i = 0; i < m.num_steps; ++i) {
            head.x += m.dx;
            head.y += m.dy;
            follow(ref head, ref tail);
            visited.Add(tail);
        }
    }

    Console.WriteLine($"Part1: Visited at least once: {visited.Count}");
}

{
    Point[] knots = new Point[10];
    HashSet<Point> visited = new();
    foreach (Move m in moves) {
        for (int i = 0; i < m.num_steps; ++i) {
            //  move head
            knots[0].y += m.dy;
            knots[0].x += m.dx;

            for (int knot_idx = 1; knot_idx < 10; ++knot_idx) {
                // use ref local to store a ref inside the array
                ref Point knot = ref knots[knot_idx];
                ref Point follow_knot = ref knots[knot_idx - 1];
                follow(ref follow_knot, ref knot);
            }
            // add last knot's pos
            visited.Add(knots[9]);
        }
    }

    Console.WriteLine($"Part2: Visited at least once: {visited.Count}");
}

// Point -> struct -> value type -> need to explicitly take a ref
void follow(ref Point head, ref Point tail) {
    Int32 dx = head.x - tail.x;
    Int32 dy = head.y - tail.y;
    Int32 manhattan_dist = Math.Abs(dx) + Math.Abs(dy);
    if (manhattan_dist <= 1) {
        // still touching
        return;
    }

    bool row_match = dy == 0;
    bool col_match = dx == 0;
    if (row_match) {
        tail.x += Math.Clamp(dx, -1, 1);
    } else if (col_match) {
        tail.y += Math.Clamp(dy, -1, 1);
    } else {
        if (manhattan_dist <= 2) {
            // still considered touching diagonally
            return;
        }
        // move at most 2 steps
        tail.y += Math.Clamp(dy, -1, 1);
        tail.x += Math.Clamp(dx, -1, 1);
    }
}

// structs in c# are always zero-inited
struct Point { public Int32 y; public Int32 x; };
struct Move { public Int32 dy; public Int32 dx; public Int32 num_steps;
 public override string ToString() {
     return $"Move dy={dy}, x={dx}, num_steps={num_steps}";
 }
};

