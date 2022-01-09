from utils import readfile

# if i strip whitespace here the tracks wont match up for the first row
inp = readfile("d13.in").splitlines()
lens = [len(r) for r in inp]
width = max(lens)

#            0    1    2    3
# facing = ("N", "W", "S", "E")
carts = {}
for y in range(len(inp)):
    for x in range(width):
        if x >= len(inp[y]):
            break
        if inp[y][x] in ("^", "v", "<", ">"):
            c = inp[y][x]
            if c == "^":
                facing = 0
            elif c == "v":
                facing = 2
            elif c == "<":
                facing = 1
            elif c == ">":
                facing = 3
            # 0 == direction index to decide if to go left straight...
            carts[(y, x)] = (facing, 0)

# direction 0 left, 1 straight, 2 right
# facing + x = facing after turning into dir
# facing N(0) direction 2 (=turning right) facing = (facing + dir_face[2]) % 4
# facing == 3 (E)
dir_face = (1, 0, -1)
# we just need to move according to the dir were facing
# not according to the track we just need to turn according to it
facing_movements = [
            # N
            (-1, 0),
            # W
            (0, -1),
            # S
            (1, 0),
            # E
            (0, 1)
        ]
#             # / and \ are not actually positioned like:
#             #  -   but like:
#             # /              /-
#             # |              |
#             # so we only ever move in either x or y
#             # so facing N on / doesnt work we need to face either E or S
# curves in track, which direction to turn to depending on facing N W S E
turn = {
        #     N  W  S  E
        #     right, left, right, left
        "/": (2, 0, 2, 0),
        "\\": (0, 2, 0, 2)
    }
# / <
# > /
# /
# ^
# v
# /

# \ <
# > \
# \
# ^
# v
# \

found = False
while True:
    moved = {}
    if len(carts) == 1:
        y, x = [k for k in carts][0]
        print("Part2:", f"{x},{y}")
        break
    # move carts
    while carts:
        # get next car to move based on lowest y then lowest x
        # sorting works with tuples
        # >>> l = [ (1, 0), (0, 2), (1, 2) ]
        # >>> sorted(l)
        # [(0, 2), (1, 0), (1, 2)]
        loc = min(carts)
        facing, direction = carts.pop(loc)
        y, x = loc
        track = inp[y][x]
        if track == "+":
            # intersection
            # modify facing depending on direction we turned into: eg. N and left = West
            facing = (facing + dir_face[direction]) % 4
            # index of direction to turn to next: 0 left, 1 straight, 2 right
            direction = (direction + 1) % 3
        elif track == " ":
            raise Exception("Cart off track")
        # under cart in intital state is straight track
        if track in ("^", "v"):
            track = "|"
        elif track in ("<", ">"):
            track = "-"
        # curves
        elif track in ("/", "\\"):
            # get which dir to turn to
            turn_dir = turn[track][facing]
            facing = (facing + dir_face[turn_dir]) % 4
        # move according to direction were facing
        moved_loc = y + facing_movements[facing][0], x + facing_movements[facing][1]
        if moved_loc[0] > len(inp) or moved_loc[1] > len(inp[moved_loc[0]]):
            print(facing, y, x, track, moved_loc)
            raise Exception("Cart off grid")

        # check for crash between every move
        if (moved_loc in moved or moved_loc in carts):
            # crash
            if not found:
                found = True
                print("Part1:", f"{moved_loc[1]},{moved_loc[0]}")
                # break in Part1
                # break
            # remove crashed carts
            moved.pop(moved_loc, 0)
            carts.pop(moved_loc, 0)
        else:
            # only assign moved cart if we didnt crash
            moved[moved_loc] = (facing, direction)
    carts = moved
