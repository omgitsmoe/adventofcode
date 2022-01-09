import re

with open("d20_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read().splitlines()

re_line = re.compile(r"p=<(.+)>, v=<(.+)>, a=<(.+)>")

parts = []
for ln in inp:
    pos, v, a = re.match(re_line, ln).groups()
    pos = [int(x) for x in pos.split(",")]
    v = [int(x) for x in v.split(",")]
    a = [int(x) for x in a.split(",")]
    parts.append((pos, v, a))

# absacc = [sum([abs(x) for x in part[2]]) for part in parts]
# maxabsacc = max(absacc)
# print(maxabsacc, absacc.index(maxabsacc))

def update(parts):
    minpos = None
    minpos_i = 0
    for i, part in enumerate(parts):
        pos, v, a = part
        # mutable type (list) so this works
        v[0] += a[0]
        v[1] += a[1]
        v[2] += a[2]
        pos[0] += v[0]
        pos[1] += v[1]
        pos[2] += v[2]

        manh_dist = sum((abs(x) for x in pos))
        # keep track of min manhattan distance
        if minpos is None:
            minpos = manh_dist
            minpos_i = i
        elif manh_dist < minpos:
            minpos = manh_dist
            minpos_i = i

    return minpos, minpos_i

def update_collide(parts):
    parts_remain = []
    # pos where collisions happend to filter particles at those positions but got added first and thus werent filtered
    collided_pos = set()
    # all pos get added here, if length doesnt change after adding -> duplicate -> collision
    positions = set()
    for i, part in enumerate(parts):
        pos, v, a = part
        # mutable type (list) so this works
        v[0] += a[0]
        v[1] += a[1]
        v[2] += a[2]
        pos[0] += v[0]
        pos[1] += v[1]
        pos[2] += v[2]

        lenb4 = len(positions)
        # length doesnt change if pos alrdy occupied
        positions.add(tuple(pos))
        if len(positions) != lenb4:
            parts_remain.append((pos, v, a))
        else:  # --> collision happend
            # remeber position and delete that particle afterwards since it also collided and should be destroyed
            collided_pos.add(tuple(pos))
    # filter particles that shouldve been destroyed (but werent since they were the first at that pos) using collided_pos
    parts_remain = [p for p in parts_remain if tuple(p[0]) not in collided_pos]

    return parts_remain



# for n in range(1000):
#     m, mi = update(parts)
# else:
#     print("part1:", m,mi)
lenb4 = len(parts)
n=0
while True:
    parts = update_collide(parts)
    # n>100 hack since firt few updates no collide
    if n>100 and len(parts) == lenb4:
        break
    else:
        n+=1
        lenb4 = len(parts)
print("part2:",n,"steps",len(parts), "remaining")

