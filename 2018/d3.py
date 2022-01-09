from utils import readfile

inp = readfile("d3_input.txt").strip().splitlines()

# WARNING shortening this to something like 1000*[1000*[None]] doesn't really
# work because you end up with 5 copies of the same list, so when you modify
# one of them they all change
fabric = [[None]*1000 for i in range(1000)]

for ln in inp:
    # id     x,y  widthXheight
    # #123 @ 3,2: 5x4
    # ...........
    # ...........
    # ...#####...
    # ...#####...
    # ...#####...
    # ...#####...
    # ...........
    id_, _, pos, size = ln.strip().split(" ")
    id_ = int(id_[1:])
    x, y = pos.split(",")
    x, y = int(x), int(y[:-1])
    width, height = size.split("x")
    width, height = int(width), int(height)
    # fill in heightXwidth fields starting from y,x
    for i in range(height):
        for j in range(width):
            cur = fabric[y+i][x+j]
            if not cur:
                fabric[y+i][x+j] = id_
            elif isinstance(cur, tuple):
                fabric[y+i][x+j] = cur + (id_,)
            else:
                fabric[y+i][x+j] = (cur, id_)

overlap = 0
overlap_ids = set()
for row in fabric:
    for ids in row:
        if isinstance(ids, tuple):
            overlap += 1
            overlap_ids.update(ids)

print("Part1:", overlap)
print("Part2:", set(range(1, len(inp)+1)) - overlap_ids)
