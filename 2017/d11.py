with open("d11_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

# inp = "ne,ne,se,s,s"
inp = inp.strip().split(",")

# dist is equal to the greatest of the absolute values of: the difference along the x-axis, the difference along the y-axis, or the difference of these two differences:
# For example, from (1, 1) to (0, 3):
# DELTAx = 0 - 1 = -1
# DELTAy = 3 - 1 = 2
# DELTAd = 2 - (-1) = 3 (greater than |2| or |-1|)
# D = |3| = 3
# since we want dist to center(0,0) we can just use greates val of absolute x or y coord of pos or their absolute diff
def dist_from_cent(pos):
    return max(abs(pos[0]), abs(pos[1]), abs((pos[0] - pos[1])))

# x,y; x increasing to right, y increasing going top
# y axis slanted to hex diagonal see: d11_easier-hex_coordinates.png
pos = [0,0]
max_dist = 0
for step in inp:
    if step == "n":
        pos[1] += 1
    elif step == "ne":
        pos[0] += 1
        pos[1] += 1
    elif step == "nw":
        # since y is slanted we stay on same y
        pos[0] -= 1
    elif step == "se":
        # since y is slanted we stay on same y
        pos[0] += 1
    elif step == "sw":
        pos[0] -= 1
        pos[1] -= 1
    elif step == "s":
        pos[1] -= 1
    max_dist = max(max_dist, dist_from_cent(pos))
dist = dist_from_cent(pos)
# ne,ne,ne is 3 steps away.
# ne,ne,sw,sw is 0 steps away (back where you started).
# ne,ne,s,s is 2 steps away (se,se). -> se,se means here 2 steps se from starting point to get to child, not other way around
# se,sw,se,sw,sw is 3 steps away (s,s,sw).
print("part1:", pos, dist)
print("part2:", max_dist)
