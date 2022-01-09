import math
from collections import namedtuple
inp = open("d10.in").read()
inp = [[c for c in ln] for ln in inp.splitlines()]
max_y = len(inp)
max_x = len(inp[0])

vec2 = namedtuple("Vec2", "x y")
EPSILON = 0.455555
max_pts = 0
max_xy = None
# solution by casting rays at angles from 0-360 from the base
# apparently doesn't work even (maybe since we don't know the size of the asteroids)
# if we do really small substeps for the angles and points on the casted line/ray
#for y in range(len(inp)):
#    for x in range(len(inp[0])):
#        if inp[y][x] == '.':
#            # can only build station on an asteroid
#            continue
#        visible = set()
#        # cast rays in 1 degree steps from the point we're testing
#        for ang in range(360 + 1):
#            # substeps per 1 degree angle
#            substeps = 20
#            step = 1/substeps
#            for i in range(substeps):
#                ang = ang + i * step
#                down_vec = vec2(0, 1)
#                # 2d vec rotation
#                cos = math.cos(ang)
#                sin = math.sin(ang)
#                x_rot = cos * down_vec.x - sin * down_vec.y
#                y_rot = sin * down_vec.x + cos * down_vec.y
#                # rotated_by_angle = vec2(x_rot, y_rot)
#                substep_line = 0.01
#                mult = 0
#                lx, ly = x, y
#                while True:
#                    mult += substep_line
#                    lx = lx + x_rot * mult
#                    ly = ly + y_rot * mult
#                    #print("pt", y, x, "ang", ang, "raypt", ly, lx)
#                    # if lx == x and ly == y:
#                    #     continue
#                    if lx < 0 or ly < 0 or lx >= max_x or ly >= max_y:
#                        break

#                    ly_rounded = round(ly)
#                    lx_rounded = round(lx)
#                    if ly_rounded >= max_y or lx_rounded >= max_x:
#                        break
#                    elif ly_rounded == y and lx_rounded == x:
#                        continue

#                    # if dist to integer point is smaller than our epsilon consider it "hit/detected"
#                    if abs(ly - ly_rounded) <= EPSILON and abs(lx - lx_rounded) <= EPSILON:
#                        if inp[ly_rounded][lx_rounded] == "#":
#                            #print("hit", ly, lx)
#                            visible.add((ly_rounded, lx_rounded))
#                            # asteroids on this ray will be blocked
#                            break
#        visible = len(visible)
#        print("VIS", x, y, visible)
#        if visible > max_pts:
#            max_pts = visible
#            max_xy = vec2(x, y)

# compute angles to all the other androids and add them to a set
# since same angle means blocked and only first can be detected
for y in range(len(inp)):
    for x in range(len(inp[0])):
        if inp[y][x] != '#':
            # can only build station on an asteroid
            continue
        visible = set()
        for ty in range(len(inp)):
            for tx in range(len(inp[ty])):
                if inp[ty][tx] != '#' or (ty == y and tx == x):
                    continue
                # calculate angle of the line from 2 points on the line
                # end - start
                dx = tx - x
                # starty - endy
                dy = y - ty
                # atan2 -> angle in radians of a (x, y) point and positive x-axis
                # radians * 180 / pi -> in deg
                angle = math.atan2(dx, dy) * 180 / math.pi
                if angle < 0:
                    angle = 360 + angle
                visible.add(angle)
        visible = len(visible)
        # print("VIS", x, y, visible)
        if visible > max_pts:
            max_pts = visible
            max_xy = vec2(x, y)
print("PART1:", max_pts, max_xy)

# part 2
asteroids = []
for ty in range(len(inp)):
    for tx in range(len(inp[ty])):
        if inp[ty][tx] != '#' or (ty == max_xy.y and tx == max_xy.x):
            continue
        # calculate angle of the line from 2 points on the line
        # (dx, dy) is basically the directed? (from start to end) slope of the line that
        # that runs through start pt x,y and end pt tx,ty
        dx = tx - max_xy.x
        dy = ty - max_xy.y
        # atan2(y, x) -> angle in radians of a (x, y) point and positive x-axis
        # radians * 180 / pi -> in deg
        # directed slope used with atan2 gives us the angle of the line from x,y to tx,ty
        # remember we are in a coordinate system where y axis direction is reversed
        # so use inverse dy
        angle = math.atan2(dy, dx) * 180 / math.pi
        if angle < 0:
            angle = 360 + angle
        # dx 1 dy 0 (directly right) gives us 0 deg angle but we want 90 deg so bias the angle by that amount
        # so 0 deg starts looking up
        angle = (angle + 90) % 360
        asteroids.append((angle, ty, tx, abs(dx) + abs(dy)))
# sort by angle AND distance since closer asteroids get destroyed first and
# blocked ones only on the next full rotation
asteroids.sort(key=lambda x: (x[0], x[3]), reverse=False)
elim = 0
last_angle = None
i = 0
# print("\n".join(str(a) for a in asteroids))
# print("\n".join(f"x {x:02d} y {y:02d} ang {ang:07.3f} dist {d:02d}" for ang, y, x, d in asteroids))
# asteroids =None
while asteroids:
    # skip asteroids with the same angle as last one since they won't
    # get destroyed until next full rotation
    if asteroids[i][0] != last_angle:
        ang, y, x, d = asteroids.pop(i)
        last_angle = ang
        elim += 1
        # print(f"{elim:03d} Destroyed: x {x:02d} y {y:02d} ang {ang:07.3f} dist {d:02d}")
        if elim == 200:
            print("PART2:", x * 100 + y)
    else:
        i += 1

    if i >= len(asteroids):
        i = 0
        last_angle = None
