from utils import readfile
inp = readfile("d10_input.txt").strip().splitlines()

# !!!dont use a grid for this since we can have overlapping points
points = []

for ln in inp:
    split = ln.split("<")
    x, y = split[1].split(", ")
    y = y.split(">")[0]
    vx, vy = split[2].split(", ")
    vy = vy.split(">")[0]
    points.append((int(x), int(y), int(vx), int(vy)))


def calc_grid_size(points):
    min_x = min(points, key=lambda x: x[0])[0]
    min_y = min(points, key=lambda x: x[1])[1]
    max_x = max(points, key=lambda x: x[0])[0]
    max_y = max(points, key=lambda x: x[1])[1]
    width = max_x - min_x
    height = max_y - min_y
    return width * height, width, height


def print_grid(points):
    min_x = min(points, key=lambda x: x[0])[0]
    min_y = min(points, key=lambda x: x[1])[1]
    max_x = max(points, key=lambda x: x[0])[0]
    max_y = max(points, key=lambda x: x[1])[1]
    # create set of x,y pts
    pts = set((p[0], p[1]) for p in points)
    for y in range(min_y, max_y+1):
        for x in range(min_x, max_x+1):
            if (x, y) in pts:
                print("#", end="")
            else:
                print(".", end="")
        print("\n", end="")


_, _, old_h = calc_grid_size(points)
sec = 0
while True:
    # move points
    moved_pts = []
    for (x, y, vx, vy) in points:
        moved_pts.append((x + vx, y + vy, vx, vy))

    # points move until they are aligned -> grid is gettinng smaller
    # and bigger again once they move away from their alignment again
    # msg (in example and as it turns out also in actual challenge) in uppercase letters
    # -> all same height
    # -> check for height getting bigger -> grid b4 contains msg
    _, _, h = calc_grid_size(moved_pts)
    if h > old_h:
        print_grid(points)
        break
    points = moved_pts
    old_h = h
    sec += 1

print("Part2:", sec)
