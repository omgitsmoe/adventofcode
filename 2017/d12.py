with open("d12_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read().strip()

inp_new = []
for ln in inp.splitlines():
    pr, cons = ln.split(" <-> ")
    inp_new.append([int(pr), [int(s) for s in cons.split(", ")]])
inp = inp_new
# inp = """0 <-> 2
# 1 <-> 1
# 2 <-> 0, 3, 4
# 3 <-> 2, 4
# 4 <-> 2, 3, 6
# 5 <-> 6
# 6 <-> 4, 5""".splitlines()
group = set([0])
# queue of programs to visit starting with 0
queue = [0]

# visit each prog in queue starting with 0, if its not in group yet add it to group and queue so we can visit it later and get its connections etc. and add those to queue and so on
while queue:
    # index in list == program name
    idx = queue.pop()
    program, cons = inp[idx]
    for con in cons:
        # not yet visited -> add to group and queue (to-visit)
        if con not in group:
            queue.append(con)
            group.add(con)

print("part1", len(group))

# working
def find(x):
    # find x in frozensets of groups set and return the subset that contains x
    for frozset in groups:
        if x in frozset:
            return frozset

# build set of frozensets containing the programs
# frozenset needs iterable -> [ln[0]]
groups = set([frozenset([ln[0]]) for ln in inp])

# loop over every prog and its cons, find subset containing prog and con -> they have to match as theyre supposed to be in the same group -> so if not add their union to groups and remove the two single sets
for program, cons in inp:
    for con in cons:
        set_prog = find(program)
        set_con = find(con)
        # prog and con have to be in the same set -> if not add them as union and remove the single sets
        if set_prog != set_con:
            groups.add(frozenset.union(set_prog, set_con))
            groups.discard(set_prog)
            groups.discard(set_con)
print(len(groups))

#working
visited = set()
groups = 0
# start at every program in input
for i in range(2000):
    # if prog alrdy visited -> skip
    if i in visited:
        continue
    # queue of programs to visit starting with i(0..1999)
    queue = [i]
    # next grp, since we walked through whole grp(s) and i wasnt visited yet
    groups += 1

    while queue:
        # index in list == program name
        idx = queue.pop()
        program, cons = inp[idx]
        for con in cons:
            # not yet visited -> add to group and queue (to-visit)
            if con not in visited:
                queue.append(con)
                visited.add(con)
print("part2:", groups)

