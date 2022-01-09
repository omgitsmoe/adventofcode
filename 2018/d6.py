from utils import readfile

inp = readfile("d6_input.txt").strip()
inp = {(int(ln.split(", ")[0]), int(ln.split(", ")[1])): i
       for i, ln in enumerate(inp.splitlines())}
# reverse access id->pos
id_pos = {id_: pos for pos, id_ in inp.items()}
areas = {i: 0 for i in range(len(inp))}
min_x = min(inp.keys(), key=lambda x: x[0])[0]
min_y = min(inp.keys(), key=lambda x: x[1])[1]
max_x = max(inp.keys(), key=lambda x: x[0])[0]
max_y = max(inp.keys(), key=lambda x: x[1])[1]

# relative grid "starting" at min_x/min_y
grid = [["." for x in range(max_x + 1)] for y in range(max_y + 1)]
# part2 same grid but total distances to all coords in input are the elements
dists = [["." for x in range(max_x + 1)] for y in range(max_y + 1)]
# loop through grid and assign id of smallest dist to pos in input
# list if multiple
for gy in range(len(grid)):
    for gx in range(len(grid[gy])):
        # relative since grid "starts" at min_y/min_x
        ry, rx = gy + min_y, gx + min_x
        dist = None
        pos_id = None
        # part2 total dist to all coords in inp
        total_dist = 0
        for (x, y) in inp:
            # manhattan distance
            tmp = abs(ry-y) + abs(rx-x)
            total_dist += tmp
            if dist is None:
                dist = tmp
                pos_id = inp[(x, y)]
            elif tmp < dist:
                dist = tmp
                pos_id = inp[(x, y)]
            elif tmp == dist:
                # same dist to multiple -> doesnt count
                if isinstance(pos_id, list):
                    pos_id.append(inp[(x, y)])
                else:
                    pos_id = [pos_id, inp[(x, y)]]
        grid[gy][gx] = pos_id
        if total_dist < 10000:
            dists[gy][gx] = "#"
        # count positions that belong to area of id
        if isinstance(pos_id, list):
            continue
        else:
            areas[pos_id] += 1
# filter infinite areas
# infinite if pos_id is present at outer rows/cols of grid
for y in (0, max_y):
    for pos_id in grid[y]:
            if isinstance(pos_id, list):
                continue
            else:
                # might be deleted alrdy
                try:
                    del areas[pos_id]
                except KeyError:
                    pass
for x in (0, max_x):
    for row in grid:
        pos_id = row[x]
        if isinstance(pos_id, list):
            continue
        else:
            # might be deleted alrdy
            try:
                del areas[pos_id]
            except KeyError:
                pass
max_area = -1
for area in areas.values():
    if area > max_area:
        max_area = area
print("Part1:", max_area)

counted = set()


def area_size(start_y, start_x, size=0):
    # fields to visit to look for sorrounding '#' fields
    to_visit = set(((start_y, start_x),))
    while to_visit:
        visit = to_visit.pop()
        size += 1 
        # so we dont count fields twice
        counted.add(visit)
        start_y, start_x = visit
        # look left, right, up, down for fields to visit
        if (start_x > 0 and dists[start_y][start_x - 1] == "#"
                and (start_y, start_x - 1) not in counted):
            to_visit.add((start_y, start_x - 1))
        if (start_x <= max_x and dists[start_y][start_x + 1] == "#"
              and (start_y, start_x + 1) not in counted):
            to_visit.add((start_y, start_x + 1))
        if (start_y > 0 and dists[start_y - 1][start_x] == "#"
              and (start_y - 1, start_x) not in counted):
            to_visit.add((start_y - 1, start_x))
        if (start_y <= max_y and dists[start_y + 1][start_x] == "#"
              and (start_y + 1, start_x) not in counted):
            to_visit.add((start_y + 1, start_x))  
    return size


max_area_ts = 0
for y, row in enumerate(dists):
    for x, c in enumerate(row):
        # so we dont count areas twice
        if (y, x) in counted:
            continue
        if c == "#":
            tmp = area_size(y, x)
            if tmp > max_area_ts:
                max_area_ts = tmp
print("Part2:", max_area_ts)
