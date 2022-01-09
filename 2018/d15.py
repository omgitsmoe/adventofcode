from utils import readfile

inp = readfile("d15.in").splitlines()
# first summarized combat example
#inp = """#######
##G..#E#
##E#E.E#
##G.##.#
##...#E#
##...E.#
########""".splitlines()
# long combat example
#inp = """#######
##.G...#
##...EG#
##.#.#G#
##..G#E#
##.....#
########""".splitlines()
inp = [[c for c in ln] for ln in inp]


class Unit:
    __slots__ = ("type", "pos", "hp", "atp")

    def __init__(self, t_name, pos):
        self.type = t_name
        self.pos = pos
        self.hp = 200
        self.atp = 3

    def __repr__(self):
        return f"<Unit: {'GOBLIN' if self.type == 'G' else 'ELF'}, pos={self.pos[0]},{self.pos[1]}>"


# N, W, S, E
steps = ((-1, 0), (0, -1), (0, 1), (1, 0))


def dist(start, end):
    return abs(start[1] - end[1]) + abs(start[0] - end[0])


def find_path(inp, searching, start, end):
    # print("============NEW PATH==============")
    visited = {start}
    # 1st step, 2nd current pos, 3rd movement costs, 4th movement + predicted costs
    paths = []
    # find starting steps
    for st in steps:
        pos = (start[0] + st[0], start[1] + st[1])
        field = inp[pos[0]][pos[1]]
        if field == ".":
            paths.append((st, pos, 1, 1 + dist(pos, end)))
        else:
            # target found
            if field not in ("#", searching.type):
                    return 0, st, start
    while paths:
        # continue from path witht minimal cost
        # enumerate list so we get (index, value) tuples
        # use lambda as key to get value[2] which is movement cost
        # result is (min_index, min_cost_value)
        min_idx, min_cost = min(enumerate(paths), key=lambda x: x[1][2])
        for st in steps:
            pos = (min_cost[1][0] + st[0], min_cost[1][1] + st[1])
            field = inp[pos[0]][pos[1]]
            if field == ".":
                # already visited -> dont go there
                if pos in visited:
                    continue
                # check if we have paths that have the same costs and current position
                # -> we only need to keep the one whose starting step is first in reading order
                new_p = (min_cost[0], pos, min_cost[2] + 1, min_cost[2] + dist(pos, end))
                same_cost_and_pos = [p for p in paths if p[1] == new_p[1] and p[2] == new_p[2]]
                if not same_cost_and_pos:
                    paths.append(new_p)
                else:
                    # paths that overlap -> only keep the one with start step first in
                    # reading order
                    first = min(same_cost_and_pos + [new_p], key=lambda x: x[1][0])
                    paths = [p for p in paths if p not in same_cost_and_pos] + [first]
                # add last visited
                visited.add(min_cost[1])
            else:
                # target found
                if field not in (".", "#", searching.type):
                    # we need to find the path with minimal costs and ending pos
                    # that comes first in reading order
                    # same total costs
                    same_costs = [p for p in paths if p[3] == min_cost[3]]
                    if len(same_costs) > 1:
                        # compare by end pos and take first by reading order
                        best = [same_costs.pop(0)]
                        for p in same_costs:
                            if p[1] == best[0][1]:
                                best.append(p)
                            elif p[1] < best[0][1]:
                                best = [p]

                        # compare by first step -> first in reading order
                        if len(best) > 1:
                            shortest = best[0]
                            for p in best:
                                if p[0] < shortest[0]:
                                    shortest = p
                            return shortest[2], shortest[0], pos
                        else:
                            return best[0][2], best[0][0], pos
                    else:
                        return min_cost[2], min_cost[0], pos
        # all neighbours added -> del path
        del paths[min_idx]
    # no possible path found
    return -1, None


def create_units(inp):
    elfs = []
    goblins = []

    for y in range(len(inp)):
        for x in range(len(inp[y])):
            if inp[y][x] == "G":
                goblins.append(Unit("G", (y, x)))
            elif inp[y][x] == "E":
                elfs.append(Unit("E", (y, x)))
    return elfs, goblins


class ElfDied(Exception):
    pass


def solve(inp, elfs, goblins, part2=False):
    rounds = 0
    end = False
    while not end:
        # units sorted by reading order
        turns_ordered = sorted(elfs + goblins, key=lambda x: x.pos)
        while turns_ordered:
            unit = turns_ordered.pop(0)
            len_steps = []
            if unit.type == "G":
                # game is over as soon as a unit CANT identify any enemies anymore
                if not elfs:
                    end = True
                    break
                for e in elfs:
                    p = find_path(inp, unit, unit.pos, e.pos)
                    # -1 -> no path found
                    if p[0] != -1:
                        len_steps.append(p)
            else:
                if not goblins:
                    end = True
                    break
                for g in goblins:
                    p = find_path(inp, unit, unit.pos, g.pos)
                    if p[0] != -1:
                        len_steps.append(p)
            if not len_steps:
                continue
            # we already get shortest path to the other unit or the path to another unit
            # if that path is faster -> we just have to find min by movement cost and by
            # pos in reading order
            shortest = len_steps.pop(0)
            for p in len_steps:
                if p[0] < shortest[0]:
                    shortest = p
                elif p[0] == shortest[0] and p[2] < shortest[2]:
                    shortest = p
                # shortest path alrdy done in find_path
                # elif p[0] == shortest[0] and p[2] == shortest[2] and p[1] < shortest[1]:
                #     shortest = p
            # move unit
            if shortest[0] != 0:
                inp[unit.pos[0]][unit.pos[1]] = "."
                unit.pos = (unit.pos[0] + shortest[1][0], unit.pos[1] + shortest[1][1])
                inp[unit.pos[0]][unit.pos[1]] = unit.type

            # attack
            # units can attack directly after moving if now one of the adjacent fields is
            # an enemy
            targets = []
            for st in steps:
                pos = (unit.pos[0] + st[0], unit.pos[1] + st[1])
                field = inp[pos[0]][pos[1]]
                if field not in (".", "#", unit.type):
                    t = [u for u in elfs + goblins if u.pos == pos]
                    targets.append(t[0])
            if targets:
                # since steps are ordered in reading order we can use min
                # to search for target to attack since it takes 1st one
                # on conflicts
                to_attack = min(targets, key=lambda x: x.hp)
                to_attack.hp -= unit.atp
                if to_attack.hp <= 0:
                    if part2 and to_attack.type == "E":
                        raise ElfDied()
                    # delete from map
                    inp[to_attack.pos[0]][to_attack.pos[1]] = "."
                    if to_attack.type == "G":
                        goblins.remove(to_attack)
                    else:
                        elfs.remove(to_attack)
                    try:
                        # might have already had its turn
                        turns_ordered.remove(to_attack)
                    except ValueError:
                        pass
        # only fully completed rounds count towards answer for part1
        if not end:
            rounds += 1
        # print interim result (zwischenergebnis)
        # print("ROUND", rounds)
        # print("\n".join("".join(c for c in ln) for ln in inp))
    return rounds * sum(u.hp for u in elfs + goblins)


# Part1 is FULLY COMPLETED ROUNDS (so the round where unit cant find an enemy doesnt count!!)
# times the sum of all the remaining unit's hitpoints
# copy input grid
# inp_solve = [row[:] for row in inp]
# print("Part1", solve(inp_solve, *create_units(inp_solve)))

atp_bonus = 0
while True:
    # copy input grid
    inp_solve = [row[:] for row in inp]

    elfs, goblins = create_units(inp_solve)
    for e in elfs:
        e.atp += atp_bonus

    try:
        outcome = solve(inp_solve, elfs, goblins, True)
    except ElfDied:
        print("Elf died with atp bonus", atp_bonus)
        atp_bonus += 1
    else:
        print("Part2:", outcome)
        break
