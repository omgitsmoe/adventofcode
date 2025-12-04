from hashlib.hasher import Hasher
from collections import Set, Dict


alias DIRS = [
    (-1,0), (-1,1), (0,1), (1,1),
    (1,0), (1,-1), (0,-1), (-1,-1)
]


fn num_adjacent_rolls(grid: Set[Pos], pos: Pos) -> Int:
    num_adjs = 0

    # NOTE: tried to use a Tuple[Pos, Pos, ...]
    # also no iter on Tuple...
    # also Tuple indices need to be comptime-known

    # materialize allows using a comptime value at runtime explicitly
    # would also work if type was ImplicitlyCopyable
    for (dy, dx) in materialize[DIRS]():
        if Pos(pos.y + dy, pos.x + dx) in grid:
            num_adjs += 1

    return num_adjs


fn removable(grid: Set[Pos]) -> List[Pos]:
    result: List[Pos] = []
    for p in grid:
        if num_adjacent_rolls(grid, p) < 4:
            result.append(p.copy())

    # ^ means move result
    return result^


# that's alot of work for just a simple Tuple / hashable type
@fieldwise_init  # NOTE: auto-generate __init__ with all fields as args
struct Pos(Hashable, Copyable, Movable, EqualityComparable):
    var y: Int
    var x: Int

    # for Equatable (part of EqualityComparable)
    fn __eq__(self, other: Self) -> Bool:
        return self.y == other.y and self.x == other.x

    # for Hashable
    fn __hash__[H: Hasher](self, mut hasher: H):
        hasher.update(self.y)
        hasher.update(self.x)


def main():
    try:
        with open("d04.in", "r") as f:
            contents = f.read()

        # Tuple is not Copyable & Movable & Hashable & EqualityComparable ....
        # grid: Set[Tuple[Int, Int]] = {}
        grid: Set[Pos] = {}
        # wow that's some bad inference...
        for y, line in enumerate[
                    List[StringSlice[origin_of(contents)].Immutable]
                ](contents.splitlines()):
            # can't iterate string slices...
            for x in range(len(line)):
                c = line[x]
                if c == '@':
                    grid.add(Pos(y, x))

        accessible = 0
        for p in grid:
            if num_adjacent_rolls(grid, p) < 4:
                accessible += 1

        print("Part1:", accessible)

        # naive solution, which is still fast enough
        # grid2 = grid.copy()
        # removed = 0
        # to_remove = removable(grid2)
        # while to_remove:
        #     for p in to_remove:
        #         removed += 1
        #         grid2.remove(p)

        #     to_remove = removable(grid2)

        # NOTE: better would be using sth like Kahn's algorithm:
        # 1. Precompute neighbor counts for every @.
        # 2. Initialize a queue with all rolls having < 4 neighbors.
        # 3. While the queue is not empty:
        #   1. Pop a roll.
        #   2. Remove it.
        #   3. For each adjacent roll:
        #       1. Decrease its neighbor count.
        #       2. If it drops below 4, push it onto the queue.
        # -> don't need to recompute neighbors
        adjacency: Dict[Pos, Int] = {}
        for p in grid:
            adjacency[p.copy()] = num_adjacent_rolls(grid, p)

        removed = 0
        queue = [p.key.copy() for p in adjacency.items() if p.value < 4]
        while queue:
            p = queue.pop()
            grid.remove(p)
            removed += 1

            # decrement ajdacency count for neighbors
            for dy, dx in materialize[DIRS]():
                neighbor = Pos(p.y + dy, p.x + dx)
                if neighbor not in grid:
                    continue

                adjacency[neighbor] -= 1

                # just dropped below 4 -> add to queue
                if adjacency[neighbor] == 3:
                    queue.append(neighbor^)

        print("Part2:", removed)
    except e:
        print("Error:", e)

