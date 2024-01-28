from collections import deque
from typing import Set, Deque
input_file = "d21_input.txt"
STEPS_TO_GET = 64 if "example" not in input_file else 6

with open(input_file, "r") as f:
    lines = [line.strip() for line in f.readlines()]
    lines = [line for line in lines if line]

Pos = tuple[int, int]
start = (0, 0)
for y in range(len(lines)):
    try:
        line = lines[y]
        x = line.index('S')
        start = (y, x)
        break
    except ValueError:
        pass
# replace by garden plot
lines[start[0]] = lines[start[0]].replace('S', '.')
print(start)


# brute force only works for example
# def find_possible_locations(max_steps: int) -> Set[Pos]:
#     end_locations: Set[Pos] = set()
#     stack: list[tuple[Pos, int]] = []
#     stack.append((start, 0))
#     while len(stack) > 0:
#         (y, x), steps = stack.pop()

#         if steps == max_steps:
#             # print('stopping due to step limit', (y, x))
#             end_locations.add((y, x))
#             continue

#         qlen_before = len(stack)
#         for dy, dx in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
#             ny, nx = (y + dy, x + dx)
#             # NOTE: can step back etc.
#             if (ny < 0 or nx < 0 or ny >= len(lines) or nx >= len(lines[0])
#                     or lines[ny][nx] == '#'):
#                 continue
#             stack.append(((ny, nx), steps + 1))
#         if len(stack) == qlen_before:
#             # noting qed, so we end here
#             # print('stopping due to no possible moves', (y, x))
#             end_locations.add((y, x))

#     return end_locations


# ex_locs = {(2, 8), (3, 1), (3, 3), (3, 5), (3, 7), (4, 0), (4, 2), (4, 8),
#            (5, 3), (5, 5), (6, 4), (6, 6), (7, 1), (7, 3), (7, 5), (9, 3)}
# assert len(ex_locs) == 16
# print('maxsteps', STEPS_TO_GET)
# possible_locs = find_possible_locations(STEPS_TO_GET)
# if ex_locs != possible_locs:
#     print("missing", ex_locs - possible_locs)
#     print("wrong", possible_locs - possible_locs)
# print("Part1:", len(possible_locs))

# NOTE: costs for the "edges" are the same so we can use bfs to find
#       the shortest path for all positions
#       then all positions with a shortest path <= STEPS_TO_GET
#       can be end locations, if the remaining steps are even
#       (so we can step back and forth and end on that pos again)


def shortest_paths(grid: list[str], start: Pos, max_steps: int) -> tuple[list[list[int]], int]:
    shortest = [[2**31 for _ in range(len(line))] for line in lines]
    q: Deque[tuple[Pos, int]] = deque()
    q.append((start, 0))
    visited: Set[tuple[Pos, int]] = set()
    end_locations: Set[Pos] = set()
    while len(q) > 0:
        (y, x), steps = q.popleft()

        # NOTE: previously did this check below when visiting neighbouring
        #       cells, but only add them when we pop them, which meant
        #       we could then later on visit positons that were already visited
        #       (since we did not check that anymore) and we would still q
        #       new positions from that position
        #       (adding them when qing also doesn't work since then it's not
        #        the shortest pos, only when we pop them from the bfs deque
        #        then we know it's the shortest path)
        # (y, x), steps = q.popleft()
        # visited.add(((y, x), steps))
        # ...
        # if (ny < 0 or nx < 0 or ny >= len(lines) or nx >= len(lines[0])
        #         or lines[ny][nx] == '#'
        #         or ((y, x), steps + 1) in visited)):
        #     continue
        if ((y, x), steps) in visited:
            continue
        visited.add(((y, x), steps))
        shortest[y][x] = steps

        if steps == max_steps:
            end_locations.add((y, x))
            continue

        for dy, dx in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
            ny, nx = (y + dy, x + dx)
            if (ny < 0 or nx < 0 or ny >= len(lines) or nx >= len(lines[0])
                    or lines[ny][nx] == '#'):
                continue
            q.append(((ny, nx), steps + 1))

    return shortest, len(end_locations)


def num_possible_end_locactions(max_steps: int) -> int:
    min_steps_grid, num_el = shortest_paths(lines, start, max_steps)
    # NOTE: not needed, since we can just take all the end locations from ^
    # (since we now use a global visited set and allow backstepping,
    #  so it doesn't actually __just__ compute the shortest paths anymore,
    #  but all ending locations)
    # but this v works as well and we could modify ^ to just compute that
    print("Part1:", num_el)
    num_possible_locs = 0
    for row in min_steps_grid:
        for steps in row:
            if steps <= max_steps:
                # reachable
                # -> we can only end on the position if
                #    the nr of remaining steps after going to that location
                #    is even -> we can step back and forth
                remaining = max_steps - steps
                if remaining % 2 == 0:
                    num_possible_locs += 1
    return num_possible_locs


print("Part1:", num_possible_end_locactions(STEPS_TO_GET))


def num_possible_end_locactions_wrapping(grid: list[str], start: Pos, max_steps: int) -> int:
    width = len(grid[0])
    height = len(grid)
    q: Deque[tuple[Pos, int]] = deque()
    q.append((start, 0))
    visited: Set[tuple[Pos, int]] = set()
    end_locations: Set[Pos] = set()
    while len(q) > 0:
        (y, x), steps = q.popleft()

        if ((y, x), steps) in visited:
            continue
        visited.add(((y, x), steps))

        if steps == max_steps:
            end_locations.add((y, x))
            continue

        for dy, dx in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
            ny, nx = (y + dy, x + dx)
            # grid is infinite now
            if (lines[ny % height][nx % width] == '#'):
                continue
            q.append(((ny, nx), steps + 1))

    return len(end_locations)


goal = 26501365
# grid dimension 131
# find solutions for 65 (half of input) 65 + 131 (half of input + grid dim)
# and 327 (half + 2*grid dim)
# -> polynomial extrapolation
# 26501365 = 202300 * 131 + 65 where 131 is the dimension of the grid
# for steps in [65, 65 + 131, 65 + 2*131]:
#     print("maxsteps", steps, "num end locs",
#           num_possible_end_locactions_wrapping(lines, start, steps))
# => maxsteps 65 num end locs 3867
#    maxsteps 196 num end locs 34253
#    maxsteps 327 num end locs 94909


# u/charr3
# "The main thing to notice for part 2 is that the grid is a square,
# and there are no obstacles in the same row/col of the starting point.
#
# Let f(n) be the number of spaces you can reach after n steps.
# Let X be the length of your input grid. f(n), f(n+X), f(n+2X), ....,
# is a quadratic, so you can find it by finding the first 3 values,
# then use that to interpolate the final answer."
def interpolate(n: int) -> int:
    a0, a1, a2 = [3867, 34253, 94909]
    b0 = a0
    b1 = a1 - a0
    b2 = a2 - a1
    return b0 + b1*n + (n*(n-1) // 2) * (b2 - b1)


rows = len(lines)
grid_reps = goal // rows  # => 202300
print("Part2:", interpolate(grid_reps))
# or wolfram alpha quadratic fit calculator
# (find coefficients for y=ax^2 + bx + c)
# x values: 0, 1, 2
# y values: 3867, 34253, 94909
# 15135 x^2 + 15251 x + 3867
# (data is perfectly fit by a 2nd degree polynomial)
# 26501365 = 202300 * 131 + 65 where 131 is the dimension of the grid
# -> 65 offset to middle of starting grid
x = 202300
print("Part2:", 15135 * x**2 + 15251 * x + 3867)

# using numpy

# u/charr3
# vandermonde = np.matrix([[0, 0, 1], [1, 1, 1], [4, 2, 1]])
# b = np.array([a0, a1, a2])
# x = np.linalg.solve(vandermonde, b).astype(np.int64)

# # note that 26501365 = 202300 * 131 + 65 where 131 is the dimension of the grid
# n = 202300
# print("part 2:", x[0] * n * n + x[1] * n + x[2])

# OR u/MediocreSoftwareEng
# points = [(i, calc(65 + i * 131)) for i in range(3)]

# def evaluate_quadratic_equation(points, x):
#     # Fit a quadratic polynomial (degree=2) through the points
#     coefficients = np.polyfit(*zip(*points), 2)

#     # Evaluate the quadratic equation at the given x value
#     result = np.polyval(coefficients, x)
#     return round(result)

# evaluate_quadratic_equation(points, 202300)

# see https://www.youtube.com/watch?v=9UOMZSL0JTg for a good explanation
# of a different solution
