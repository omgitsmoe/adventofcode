from collections import deque
from utils import readfile

inp = readfile("d17.in").strip().splitlines()
# inp = """x=495, y=2..7
# y=7, x=495..501
# x=501, y=3..7
# x=498, y=2..4
# x=506, y=1..2
# x=498, y=10..13
# x=504, y=10..13
# y=13, x=498..504""".splitlines()

coords = {}
for ln in inp:
    single, mult = ln.split(", ")
    s_ctype, s_val = single.split("=")
    s_val = int(s_val)
    m_ctype, m_val = mult.split("=")
    m_start, m_end = m_val.split("..")
    m_range = range(int(m_start), int(m_end) + 1)
    for m_coord in m_range:
        if m_ctype == "y":
            coords[(m_coord, s_val)] = "#"
        else:
            coords[(s_val, m_coord)] = "#"
min_y = min(coords, key=lambda x: x[0])[0]
max_y = max(coords, key=lambda x: x[0])[0]

# spring starts at 0, 500 but we disregard everything above min y
to_visit = deque([(min_y - 1, 500)])
while to_visit:
    y, x = to_visit.popleft()
    if y >= max_y:
        continue
    # separate queues for decending downwards and traveling horizontally
    to_visit_row = deque([(y, x)])
    # fields visited horizontally so we dont infinte loop and can later assign ~ to standing water
    visited_row = set([(y, x)])
    blocked_left, blocked_right = False, False
    while to_visit_row:
        ry, rx = to_visit_row.popleft()
        # print("\n".join("".join(coords.get((cy, cx), ".") for cx in range(x-25, x+26)) for cy in range(y-25, y+26)))
        # print("=========================================")
        # peek below
        if coords.get((ry + 1, rx)) in (None, "|"):
            coords[(ry + 1, rx)] = "|"
            # if we appendleft here we would halt on the other branches till this one is done
            to_visit.append((ry + 1, rx))
            # if we can move downwards we dont need to travel horizontally
            continue

        right = coords.get((ry, rx + 1))
        # None -> "."/Sand so we add it to queue; if its floating water "|" we only visit
        # it if we havent already (theres probably a better way since we might visit some fields
        # mutltiple times but prob wouldnt happen since we always finsh visting a row first
        # so the fields would all be ~, we only encounter | from other branches)
        if right is None or (right == "|" and (ry, rx + 1) not in visited_row):
            coords[(ry, rx + 1)] = "|"
            to_visit_row.append((ry, rx + 1))
            visited_row.add((ry, rx + 1))
        elif right == "#":
            blocked_right = True

        left = coords.get((ry, rx - 1))
        if left is None or (left == "|" and (ry, rx - 1) not in visited_row):
            coords[(ry, rx - 1)] = "|"
            to_visit_row.append((ry, rx - 1))
            visited_row.add((ry, rx - 1))
        elif left == "#":
            blocked_left = True

    # trapped water -> mark as standing water "~"
    if blocked_left and blocked_right:
        for water_standing in visited_row:
            coords[water_standing] = "~"
        # continue with spreading the water from where we came from
        to_visit.appendleft((y - 1, x))
# we only count water fields between (including) min_y and max_y
# since we also have the clay fields "#" in coords we need to filter them
print("Part1:", sum(1 for (y, x), v in coords.items() if min_y <= y <= max_y and v != "#"))
# for printing the map
min_x = min(coords, key=lambda x: x[1])[1]
max_x = max(coords, key=lambda x: x[1])[1]
with open("d17_out.txt", "w") as f:
    f.write("\n".join("".join(coords.get((cy, cx), ".") for cx in range(min_x, max_x+1)) for cy in range(min_y, max_y+1)))

print("Part2:", sum(1 for (y, x), v in coords.items() if min_y <= y <= max_y and
                    v not in ("#", "|")))
