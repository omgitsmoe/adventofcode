from utils import readfile

inp = readfile("d8_input.txt").strip().split(" ")

class Node:
    __slots__ = ("node_id", "nr_children", "children", "nr_metadata", "metadata", "ci_for_sum")
    def __init__(self, node_id, nr_children, nr_metadata):
        self.node_id = node_id
        self.nr_children = nr_children
        self.nr_metadata = nr_metadata
        self.children = []
        self.metadata = []
        self.ci_for_sum = []
    
    def __repr__(self):
        return f"<Node({self.node_id}, {self.nr_children} childs: {' '.join(str(n.node_id) for n in self.children)}, {self.nr_metadata} metadata: {self.metadata}, CISumIDs: {self.ci_for_sum}>"


tree = {}
# node id counter
node_id = 0
parents = [] 
# list index
i = 0
while i < len(inp):
    # current parent node or current node when were parsing the metadata
    recent = parents[-1] if parents else None

    if recent is None or len(recent.children) < recent.nr_children:
        node = Node(node_id, int(inp[i]), int(inp[i+1]))
        tree[node_id] = node
        node_id += 1
        i += 2
        if recent:
            # if we have a parent node
            recent.children.append(node)
        if node.nr_children or node.nr_metadata:
            # there are children or metadata present that we need to parse
            parents.append(node)
    elif len(recent.metadata) < recent.nr_metadata:
        recent.metadata.append(int(inp[i]))
        i += 1
        # node done
        if recent.nr_metadata == len(recent.metadata):
            # part2 node_ids of valid indices for child sums
            # !!! 1-indexing -> 1 is first child of node; 0 is invalid
            recent.ci_for_sum = [recent.children[i - 1].node_id for i in recent.metadata if i != 0 and i <= recent.nr_children]
            del parents[-1]
print("Part1:", sum(sum(n.metadata) for n in tree.values()))

sums = {}
to_visit = [0]
value = 0
while to_visit:
    n = tree[to_visit[-1]]
    if n.children:
        # all necessary child sums present
        if all(c_node_id in sums for c_node_id in n.ci_for_sum):
            # child nodes can be referenced multiple times and count towards value each time
            sums[n.node_id] = sum(sums[c_nid] for c_nid in n.ci_for_sum)
            del to_visit[-1]
        else:
            # not possible to calc sum yet so dont remove node and
            # append children needed for to calc the nodes sum afterwards
            to_visit.extend(n.ci_for_sum)
    else:
        sums[n.node_id] = sum(n.metadata)
        del to_visit[-1]
    
print("Part2:", sums[0])
