from collections import deque, defaultdict

from utils import readfile

inp = readfile("d25.in").strip().splitlines()
# inp = """-1,2,2,0
# 0,0,2,-2
# 0,0,0,-2
# -1,2,0,0
# -2,-2,-2,2
# 3,0,2,-1
# -1,3,2,2
# -1,0,-1,0
# 0,2,1,-2
# 3,0,0,0""".splitlines()
inp = [tuple(int(d) for d in ln.split(",")) for ln in inp]

# we can either use the points directly here and store and edge with one point
# as key and the other as the value or we can just use their index (which we wouldnt need a
# defaultdict for) and then later only work with the indices
# edges = [set() for _ in range(len(inp))]
edges = defaultdict(set)
for p1 in inp:
    for p2 in inp:
        dist = sum(abs(p1[i] - p2[i]) for i in range(4))
        if dist <= 3:
            edges[p1].add(p2)
visited = set()
q = deque()
constellations = 0
# abort when weve visited all points
while len(visited) != len(inp):
    # queue a new point
    q.append([p for p in inp if p not in visited][0])
    constellations += 1
    # explore all points of the constellation with a bfs
    while q:
        p1 = q.popleft()
        for p2 in edges[p1]:
            if p2 in visited:
                continue
            q.append(p2)
            # inefficient to recompute the distance every time -> better to compute
            # the edges (as in the connections that are available from a point) beforehand
            # and just add all the points we have edges/connections to to the queue
            # manhattan dist
            # dist = sum(abs(p1[i] - p2[i]) for i in range(4))
            # if dist <= 3:
            #     q.append(p2)
        visited.add(p1)
print("Part1:", constellations)
