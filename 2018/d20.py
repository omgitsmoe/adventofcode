from collections import deque, defaultdict

from utils import readfile

inp = readfile("d20.in").strip()[1:-1]
# inp = "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))" # 31
# inp = "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))" # 23
# inp = "ENWWW(NEEE|SSE(EE|N))" # 10
# inp = "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN" # 18
# inp = "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NEJSKSK|AJKSDJA(XJKAJKA|KAJKA|ORLL)|)(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"
# inp = "EESS(NNSE|)SSS(NNWS|)ENN"

DIRS, PAREN_OPENED, PAREN_INSIDE, PAREN_CLOSED, OR = 0, 1, 2, 3, 4


class Node:
    node_counter = 0

    def __init__(self):
        self.id = Node.node_counter
        Node.node_counter += 1
        self.children = []
        self.directions = ""

    def __repr__(self):
        return f"Node<{self.id}, {self.directions}, childs={[str(c.id) + '/' + c.directions for c in self.children]}>"


# build tree with first node as root
root = Node()
parents = []
state = DIRS
current = root
for c in inp:
    if state == PAREN_CLOSED and c not in "()|":
        # lvl done -> new node needed if we encounter normal chars
        current = Node()
        for child in parents.pop():
            child.children.append(current)
        state = DIRS
    elif state == PAREN_CLOSED:
        # end of "branch" -> ) but no directions chars after -> dont need to append next node
        # to children of nodes of that paren
        parents.pop()

    if c == "|":
        state = OR
        # new node as child of parent that is one lvl above these parentheses
        current = Node()
        parents[-1].children.append(current)
        continue
    elif c == "(":
        state = PAREN_OPENED
        # new lvl -> add current as parent
        # at least one variation of normal (not ^$|()) chars has to follow -> new node
        parents.append(current)
        current = Node()
        parents[-1].children.append(current)
        continue
    elif c == ")":
        # lvl done -> new node needed if we encounter normal chars
        state = PAREN_CLOSED
        parents[-1] = parents[-1].children
        continue

    current.directions += c

print(root)
# vv would work if i created nodes for every position/room
# edges saved in adjacency matrix
# adj_matrix[i][j] = 1 -> edge from i to j
# adj_matrix = [[0 for j in range(5000)] for i in range(5000)]
# parents = []
# # complex number instead of using separate x and y
# pos = 0 + 0j
# directions = {"N": 1+0j, "S": -1+0j, "W": 0-1j, "E": 0+1j}
# # doors
# edges = defaultdict(set)


# traverse along tree queueing all children we find and saving the nodes we
# visited to get there as a tuple
# combine the direction strings of the nodes if we encounter end node without children
# just the longest regex isnt also the longest path to a room since there can be circular (NEWS|)
# paths
# -> build complete paths (till node has no children) and then walk the routes to build the edges
max_route = ""
q = deque()
q.append((root, (root,)))
visited = set()
# complex number instead of using separate x and y
directions = {"N": -1+0j, "S": 1+0j, "W": 0-1j, "E": 0+1j}
edges = defaultdict(set)
while q:
    # continue with next node at start!!! of queue
    # otherwise we'll get missing edges due to continuing paths to the end first (depth first)
    # (e.g. we'll miss edges here EESS(NNSE|)SSS(NNWS|)ENN)
    current, nodes = q.popleft()
    # dont queue children of already visited nodes -> otherwise duplicate routes
    if current in visited:
        continue
    # print(current, [n.id for n in nodes])
    if current.children:
        # queue children
        for c in current.children:
            # nodes in order to get to that child
            node_tuple = nodes + (c,)
            q.append((c, node_tuple))
        # add to visited if we queued the children; -> also dont add endnodes to visited
        visited.add(current)
    else:
        # no children -> end node
        route = "".join(n.directions for n in nodes)
        # print(route)
        # walk the route building the edges/doors
        current = 0+0j
        for c in route:
            # if we e.g. move north we have a door from current to new and the other way around
            # where new is -1 in y direction from the current room
            new = current + directions[c]
            edges[current].add(new)
            edges[new].add(current)
            current = new

print("EDG", sum(len(e) for e in edges.values()))
q = deque()
q.append((0, 0+0j))
visited = set()
min_1k_far = 0
while q:
    # continue with next room at start!!! of queue
    # doors travelled from center room, position
    depth, pos = q.popleft()
    if depth >= 1000:
        min_1k_far += 1
    # add all connections to the end of the queue with depth incremented by 1
    q.extend([(depth + 1, p) for p in edges[pos] if p not in visited])
    # mark rooms as visited
    visited.update(edges[pos])
print("Part1:", depth)
print("Part2:", min_1k_far)
