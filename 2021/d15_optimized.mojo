import time
from collections import Set
from hashlib.hasher import Hasher

# NOTE: converted from Python using ChatGPT
#       then fixed all errors/outdated syntax
# WARN: (opt) versions is optimized by using a 1d vec instead of nested for grid and visited set
# all versions exactly follow the algorithm used by d15.py to be comparable
# with I/O + parsing and part1:
#   python: ~470ms
#   cpp: ~156ms (clang++ -std=c++17 -O3 -march=native -o d15_cpp d15.cpp)
#   cpp(opt): ~19.3ms (g++ -std=c++17 -O3 -march=native -o d15_optimized_cpp d15_optimized.cpp)
#   cpp(opt): ~18ms (clang++ -std=c++17 -O3 -march=native -o d15_optimized_cpp d15_optimized.cpp)
#   mojo: ~130ms (mojo build d15.mojo --march=native --optimization-level=3)
#   mojo(opt): ~103ms (mojo build d15_optimized.mojo --march=native --optimization-level=3)
# without I/O:
#   python: ~360ms
#   cpp: ~200ms (g++ -std=c++17 -O3 -march=native -o d15_cpp d15.cpp)
#   cpp: ~145ms (clang++ -std=c++17 -O3 -march=native -o d15_cpp d15.cpp)
#   cpp(opt): ~16.34ms (g++ -std=c++17 -O3 -march=native -o d15_optimized_cpp d15_optimized.cpp)
#   cpp(opt): ~14.95ms (clang++ -std=c++17 -O3 -march=native -o d15_optimized_cpp d15_optimized.cpp)
#   mojo: ~108ms (mojo build d15.mojo --march=native --optimization-level=3)
#   mojo(opt): ~83ms (mojo build d15_optimized.mojo --march=native --optimization-level=3)
#
# => seems like Mojo can do some more aggressive optimizations or has some
#    stdlib types that are optimized more, otherwise the big win
#    for C++ after switching to 1d makes no sense, since Mojo only gains
#    a slight improvement


# ------------------------------------------------------------
# Hashable 2D position
# ------------------------------------------------------------
@fieldwise_init
struct Pos(Hashable, Copyable, Movable, EqualityComparable):
    var y: Int
    var x: Int

    fn __eq__(self, other: Self) -> Bool:
        return self.y == other.y and self.x == other.x

    fn __hash__[H: Hasher](self, mut hasher: H):
        hasher.update(self.y)
        hasher.update(self.x)

# ------------------------------------------------------------
# Node
# ------------------------------------------------------------
struct Node(Copyable, Movable):
    var level: Int
    var risk_sum: Int
    var prev: Pos

    def __init__(out self, level: Int, risk_sum: Int):
        self.level = level
        self.risk_sum = risk_sum
        self.prev = Pos(-1, -1)

# ------------------------------------------------------------
# Read input
# ------------------------------------------------------------
def read_file_to_lines(path: String) -> List[String]:
    with open(path, "r") as f:
        data = f.read()
    return [String(s) for s in data.splitlines()]


fn idx(y: Int, x: Int, width: Int) -> Int:
    return y * width + x


# ------------------------------------------------------------
# Binary search insertion index (descending sort by cost)
# ------------------------------------------------------------
def binsearch(arr: List[Tuple[Pos, Int]], x: Int) -> Int:
    first = 0
    last = len(arr) - 1

    while first < last:
        mid = first + (last - first) // 2
        val = arr[mid][1]

        if val == x:
            return mid
        elif val > x:
            if first == mid:
                return last if arr[last][1] < x else last + 1
            first = mid
        else:
            last = mid

    return 0

# ------------------------------------------------------------
# Pathfinding
# ------------------------------------------------------------
def find_path(mut grid: List[Node], dimy: Int, dimx: Int, start: Pos, end_pos: Pos) -> Int:

    fn in_bounds(p: Pos) -> Bool:
        return p.x >= 0 and p.x < dimx and p.y >= 0 and p.y < dimy

    var q: List[Tuple[Pos, Int]] = [(start.copy(), 0)]
    var visited = List(length=dimy * dimx, fill=False)

    while len(q) > 0:
        # (pos, path_cost) = q.pop()
        tmp = q.pop()
        ref pos = tmp[0]
        ref path_cost = tmp[1]
        (x, y) = (pos.x, pos.y)

        if pos == end_pos:
            return path_cost

        visited[idx(y, x, dimx)] = True

        # BUG/BROKEN: neighbours will always be empty after 2nd assignment
        # neighbours = [
        #     Pos(y - 1, x), Pos(y, x + 1), Pos(y + 1, x), Pos(y, x - 1)
        # ]
        # neighbours = [p.copy() for p in neighbours if in_bounds(p)]
        neighbours = [
            p.copy()
            for p in [
                Pos(y - 1, x),
                Pos(y, x + 1),
                Pos(y + 1, x),
                Pos(y, x - 1)
            ] if in_bounds(p)]

        for npos in neighbours:
            if visited[idx(npos.y, npos.x, dimx)]:
                continue

            ref neighbour_node = grid[idx(npos.y, npos.x, dimx)]
            new_cost = path_cost + neighbour_node.level

            if new_cost < neighbour_node.risk_sum:
                neighbour_node.risk_sum = new_cost
                neighbour_node.prev = pos.copy()

                idx = binsearch(q, new_cost)
                q.insert(idx, (npos.copy(), new_cost))

    raise Error("Unreachable!")

# ------------------------------------------------------------
# Main
# ------------------------------------------------------------
fn main():
    try:
        lines = read_file_to_lines("d15.in")
        dimy = len(lines)
        dimx = len(lines[0])

        # Initialize grid
        # need to manually do this conversion FROM STRING to byte then bias
        zero_ascii = UInt8('0'.as_bytes()[0])
        var grid: List[Node] = [
            Node(Int(c - zero_ascii), Int.MAX) for line in lines for c in line.as_bytes()
        ]

        print(
            "Part1:",
            find_path(grid, dimy, dimx, Pos(0, 0), Pos(dimy - 1, dimx - 1)))

        # Part2: expand 5x
        var five_x_grid: List[Node] = []
        five_x_width = dimx * 5
        five_x_height = dimy * 5
        for yy in range(5):
            for line in lines:
                for xx in range(5):
                    for c in line.as_bytes():
                        new_level = (Int(c - zero_ascii) - 1 + xx + yy) % 9 + 1
                        five_x_grid.append(Node(new_level, Int.MAX))

        start = time.perf_counter()
        print(
            "Part2:",
            find_path(five_x_grid, five_x_width, five_x_height,
                      Pos(0, 0), Pos(five_x_height - 1, five_x_width - 1)))
        end = time.perf_counter()

        took_ms = (end - start) * 1000
        print("Took:", String(took_ms), "ms")
    except e:
        print("Exception:", e)
