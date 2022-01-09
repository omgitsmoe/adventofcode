inp = open("d06.in", "r").read().splitlines()
# obj name as key and 2-tuple of directly orbited object and indirect objs
obj = {"COM": ("", [])}
from collections import defaultdict
# list of orbitor for object name as key
orbs = defaultdict(list)

# build relationships on first pass
for ln in inp:
    # AAA)BBB means BBB orbits around AAA
    orbited, orbitor = ln.split(")")
    obj[orbitor] = (orbited, [])
    orbs[orbited].append(orbitor)  # for part2, tuple only so we have same structure

for k, (direct, indirect) in obj.items():
    # append all previous direct orbits to indirect of this orbitor
    while True:
        try:
            direct, _ = obj[direct]
        except KeyError:
            break
        if direct == "":
            break  # reached COM
        indirect.append(direct)
# since we added COM obj at top but it doesnt orbit anything
# we have to subtract one from nr of orbitors in obj
print("PART1:", len(obj)-1 + sum(len(o[1]) for o in obj.values()))

from collections import deque
start = obj["YOU"][0]
target = obj["SAN"][0]
# 2tuple of current node and orbital transfers
q = deque([(start, "YOU", 0)])
min_transfers = 10e13
print("START:", start, "TARGET:", target)

while q:
    obj_name, last_obj, transfers = q.popleft()
    # print(obj_name, path, transfers, len(q))
    if obj_name in ("YOU", "COM"):
        continue
    elif obj_name == target and transfers < min_transfers:
        min_transfers = transfers
        continue
    orbitors = orbs.get(obj_name, ())
    for orbitor in orbitors:
        if orbitor != last_obj:
                q.append((orbitor, obj_name, transfers+1))
    orbited = obj.get(obj_name, None)
    if orbited and orbited[0] != last_obj:
            q.append((orbited[0], obj_name, transfers+1))

print("PART2:", min_transfers)

class Node:
    def __init__(self, value):
        self.value = value
        self.children = []

class Graph:
    def __init__(self):
        self.children_total = 0
        self.starting_node = Node("COM")
        self.nodes = [self.starting_node]
