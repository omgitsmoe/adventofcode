with open("d24_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

data = []
for ln in inp.splitlines():
    a, b = ln.split("/")
    data.append((int(a), int(b)))
    
def get_matching(portnr, components):
    return [comp for comp in components if portnr in comp]


# returns list of matching (unused portnumber matches any portnumber - since order isnt importent - of component in list) components
def get_matching_i(portnr, components):
    return [i for i, comp in enumerate(components) if portnr in comp]

def build_recursive(comp_i, port_i_used, used, solutions, components):
    # order of ports isnt important -> 0/33 can be matched by 28/0
    # but a port can only be used once -> search matching with unused port
    # port_i_used -> index of used port -> either 0 or 1 -> not port_i_used -> swaps between 0 or 1
    port_unused = components[comp_i][not port_i_used]
    matching = get_matching_i(port_unused, components)
    matching_unused = [match for match in matching if match not in used]
    #print(components[comp_i], used, matching_unused)
    if matching_unused:
        for match_i in matching_unused:
            # build bridge for all matching components
            # port used == index of port_unused, since we searched for match with that port
            # build used list with concat -> new list
            bridge = build_recursive(match_i, components[match_i].index(port_unused), used + [match_i], solutions, components)
            # if there were unused matches and the loop is finished (having appended all states as possible solutions) build_recursive will return None
            if bridge:
                solutions.append(bridge)
    else:
        # return bridge configuration if there are no more matches
        return [components[i] for i in used]


# simplfied -> only using open port and current state of bridge (used) as variable params
# -> same return type (list of lists)
# -> and able to start from 0 as open port
def build_recursive_2(open_end_port, used, components):
    # open_end_port -> unused port of last component of bridge (used[-1])
    matching_unused = [match for match in get_matching_i(open_end_port, components) if match not in used]
    solutions = []
    if matching_unused:
        for match_i in matching_unused:
            match_p1, match_p2 = components[match_i]
            # determine open port -> used port == open port of func params
            match_open_end_port = match_p1 if match_p2 == open_end_port else match_p2
            bridges = build_recursive_2(match_open_end_port, used + [match_i], components)

            # solution list returned always -> extend current list by bridge combinations of returned lists
            solutions.extend(bridges)
    else:
        # also return list of lists (of tuples) here so we have same return type
        solutions.append([components[i] for i in used])

    return solutions

solutions = build_recursive_2(0, [], data)
# build all possible bridge combinations for all (here: 3-> 0/33 at i45, 50/0 at i18, 28/0 at i20) starting points
# sol_old = []
# build_recursive(18, 1, [18], sol_old, data)
# build_recursive(20, 1, [20], sol_old, data)
# build_recursive(45, 0, [45], sol_old, data)
# print(len(solutions), len(sol2))


# sum of components (sum of ports)
strengths = [sum((sum(tup) for tup in sol)) for sol in solutions]
max_s = max(strengths)
max_si = strengths.index(max_s)
print("part1:", strengths[max_si])

# len of briges
lengths = [len(sol) for sol in solutions]
max_l = max(lengths)
# brige configurations with longest length (if there are multiple)
max_l_bridges = [solutions[i] for i, l in enumerate(lengths) if l==max_l]
# strength of longest bridges
strength_longest = [sum((sum(tup) for tup in sol)) for sol in max_l_bridges]
print("part2:", max(strength_longest))
