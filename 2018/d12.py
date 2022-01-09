from utils import readfile

inp = readfile("d12_input.txt").strip().splitlines()

state = inp.pop(0).split(": ")[1]
# empty line
del inp[0]

# state = "#..#.#..##......###...###"
# inp = """...## => #
# ..#.. => #
# .#... => #
# .#.#. => #
# .#.## => #
# .##.. => #
# .#### => #
# #.#.# => #
# #.### => #
# ##.#. => #
# ##.## => #
# ###.. => #
# ###.# => #
# ####. => #""".splitlines()

rules = {}
for ln in inp:
    old, ngen = ln.split(" => ")
    rules[old] = ngen


def get(i):
    if i < 0:
        return "."
    if i >= len(state):
        return "."
    return state[i]


leftmost = 0
for n in range(200):
    print(n, "\r", end="", flush=True)
    new = []
    # 4 3 2 1 [#
    # l l c r  r
    #     ^ look from here
    # start looking 2 pots to the left of current leftmost pot
    # for rules like e.g. this: .#... => #
    # and 2 pots to the right
    for i in range(-2, len(state) + 2):
        l2, l1, c, r1, r2 = get(i-2), get(i-1), get(i), get(i+1), get(i+2)
        plant = rules["".join((l2, l1, c, r1, r2))]
        # print(l2,l1,c,r1,r2)
        # print(plant)
        # left of current leftmost
        if i < 0:
            if plant == "#":
                new.append(plant)
                leftmost -= 1
        # beyond current rightmost
        elif i >= len(state):
            if plant == "#":
                new.append(plant)
        else:
            new.append(plant)
    state = new
    # print(n, len(state), state.count("#"), sum(i+leftmost for i, c in enumerate(state) if c == "#"))
    # print("".join(state))


# item 0 in list is pot nr leftmost
print("Part1:", sum(i+leftmost for i, c in enumerate(state) if c == "#"))
# after 143rd (0 being 1st gen after initial state) iteration it starts to cycle
# adding 32 to the score for every iteration
# 144 (==145th gen) score 5041
# 5e10 -> 50 000 000 000
# 5e10 - 145 iterations left, starting from score of that 145th gerneration adding 32
# each time
print("Part2:", 5041 + 32 * (int(5e10) - 145))
