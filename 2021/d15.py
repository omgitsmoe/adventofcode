import sys
import bisect

from typing import Tuple, Set, List
# open exectutes the function passed as first param and then closes the file
with open("d15.in", "r") as f:
    contents = f.read().strip().splitlines()

class Node:
    level: int
    risk_sum: int
    prev: Tuple[int, int]
    def __init__(self, level, risk_sum) -> None:
        self.level = level
        self.risk_sum = risk_sum
        self.prev = (-1, -1)


Pos = Tuple[int, int]

grid = [[Node(int(c), sys.maxsize) for c in line] for line in contents]
dimy = len(grid)
dimx = len(grid[0])

# search linearly from the end of the arr (so we are closer to haskell solution)
def insert_index(arr, x):
    for i in range(len(arr) - 1, 0 - 1, -1):
        (_, y) = arr[i]
        if x <= y:
            return i + 1
    return 0

def binsearch(arr, x):
    last = len(arr) - 1
    first = 0

    while first < last:
        mid = first + (last - first) // 2
        # hard coded 2nd element which is the cost here
        val = arr[mid][1]
        # inserting 2
        # 54331
        # f ^ l
        # 54331
        #   f^l
        # 54331
        #    fl -> first == mid
        if val == x:
            return mid
        # mid value is higher, use mid as new start of the search window
        elif val > x:
            # mid will fall onto first when last/first are right next to eachother
            # so if they are the same and the value is still higher we can
            # return last (since we want a descending sort, so smallest number is at
            # the end of the arr -> we have to be __at least__ one to the right of
            # first/mid which is last)
            if first == mid:
                # check if last is smaller -> insert at/before (insert moves the
                # element at inseted idx one to the right)
                # last is greater -> one to the right of last
                return last if arr[last][1] < x else last + 1
            first = mid
        else:
            # val at mid is lower move end of the search window to current mid
            last = mid

    # reached start of arr (or [] or [y]) and no value was greater
    return 0

def find_path(grid: List[List[Node]], start: Pos, end_pos: Pos) -> int:
    # 2nd param is dimension
    dimy = len(grid)
    dimx = len(grid[0])
    # sorted: last item should be the one with the lowest riskSum
    q: List[Tuple[Pos, int]] = [(start, 0)]
    visited: Set[Pos] = set([start])

    while q:
        pos, path_cost = q.pop()
        (x, y) = pos

        if pos == end_pos:
            return path_cost

        # mark visited
        visited.add(pos)
        neighbours = filter(lambda xy: xy[0] >= 0 and xy[0] < dimx and xy[1] >= 0 and xy[1] < dimy,
                            [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)])
        for npos in neighbours:
            if npos in visited:
                # should already have optimal cost
                continue

            nx, ny = npos
            neighbour_node = grid[ny][nx]
            new_cost = path_cost + neighbour_node.level
            if new_cost < neighbour_node.risk_sum:
                # cheaper path -> update in grid and queue
                neighbour_node.risk_sum = new_cost
                neighbour_node.prev = pos

                # old linear search:
                # q.insert(insert_index(q, new_cost), (npos, new_cost))
                insert_index = binsearch(q, new_cost)
                q.insert(insert_index, (npos, new_cost))

print(find_path(grid, (0, 0), (len(grid[0]) - 1, len(grid) - 1)))

import time
start = time.time()
five_x_grid = [
    [
        Node((int(c) - 1 + xx + yy) % 9 + 1, sys.maxsize)
        for xx in range(5) for c in line
    ]
    for yy in range(5) for line in contents
]
print("Part2:", find_path(five_x_grid, (0, 0), (len(five_x_grid[0]) - 1, len(five_x_grid) - 1)))
end = time.time()
took_secs = end - start
took_ms = took_secs * 1000
print("Took:", took_secs, "s", "->", took_ms, "ms")
# python: 1.5s (with binsearch; without 6.7s)
# julia 0.4s with same binsearch as python (own impl); julia 5s (with stdlib bin search 0.5s)
# mikefarquhar's rust solution: 30ms
# toakushi's go solution: 184ms Dijkstra (A* 214ms)
# tpatetl's nodejs solution: 224s
# heyitsmattwade's nodejs solution: 1.5s (using a heap/prioqueue library)
# IT WAS ACTUALLY IMPOSSIBLE TO FIND A JS IMPLEMENTATION THAT DID NOT use
# A LIBRARY!!_)!!_)_!_!__!_!_!! HOW? what's the point of AoC then
# 3/5 used a lib for dijkstra which IS THE WHOLE CODING TASK!?!??!
# can't use binsearch with Haskell since the q is a list and iterating over the list
# is what's slow
# Haskell 728.1910445s
# F#: ~13s (with dotnet fsi d15.fsx command which apparently gets compiled)
# (^ also no binsearch, and using the same algo as Haskell, the ONLY difference is that
#  F# uses the mutable array whereas Haskell uses the immutable Data.Array implementation)
