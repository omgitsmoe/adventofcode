from collections import defaultdict, deque

directions, edges = {'N': 0 + 1j, 'E': 1 + 0j, 'S': 0 - 1j, 'W': -1 + 0j}, defaultdict(set)
with open('d20.in') as puzzle_file:
    close_stack, open_stack, current = [], [], {0 + 0j}
    for c in puzzle_file.read().strip().lstrip('^').rstrip('$'):
        if c in directions:
            subsequent = [p + directions[c] for p in current]
            for p, q in zip(current, subsequent):
                edges[p].add(q)
                edges[q].add(p)
            current = subsequent
        elif c == '(':
            close_stack.append(set())
            open_stack.append(current)
        elif c == '|':
            close_stack[-1].update(current)
            current = open_stack[-1]
        elif c == ')':
            close_stack[-1].update(current)
            open_stack.pop()
            current = close_stack.pop()

print("EDG", sum(len(e) for e in edges.values()))
far_count, queue, seen = 0, deque(((0, 0 + 0j), )), set()
while queue:
    depth, p = queue.popleft()
    far_count += depth >= 1000
    queue.extend((depth + 1, q) for q in edges[p] if q not in seen)
    seen.update(edges[p])
print(depth)
print(far_count)
