with open("d13_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read().strip()
# inp = """0: 3
# 1: 2
# 4: 4
# 6: 4"""
# no range info for every depth
inp = {int(ln.split(": ")[0]): [None if n != 0 else "S" for n in range(int(ln.split(": ")[1]))] for ln in inp.splitlines()}

severity = 0
# old: for getting max of list of first value of tuples [(depth, [S,None,None]), (depth, [..
# get every first val of tuples in inp with map(lambda x: x[0], inp) -> then get max
# map(lambda x: x[0], inp)
# as we change inp -> make copy first with .copy()
# .copy() doesnt work, since only shallow copy -> stackoverflow: 
# Also note that the dict.copy() is shallow, if there is a nested list/etc in there changes will be applied to both. IIRC. Deepcopy will avoid that.
# For any dictionary I ever work with, deepcopy is what I need... I just lost several hours due to a bug that was because I wasn't getting a complete copy of a nested dictionary and my changes to nested entries were affecting the original
# use:
# import copy
# dict2 = copy.deepcopy(dict1)
import copy
inp_cp = copy.deepcopy(inp)
for pos in range(max(inp_cp)+1):
    # to be caught Scanner has to be at pos[0] b4 we move scanners
    caught = False
    try:
        caught = inp_cp[pos][0] == "S"
        if caught:
            caught_once = True
            severity += (pos * len(inp_cp[pos]))
    except KeyError:
        pass
    # print(pos, caught, severity, "\n", inp_cp)
    # advance scanner in all layers
    for l in inp_cp.values():
        scanner = l.index("S")
        l[scanner] = None
        # scanner gehen vor und zurück, immer einen step -> pos==anzahl schritte
        # schritte bis wieder an 0 position = (len(l)-1)*2 oder range*2-2
        # => dadurch muss man die liste gar nicht programmieren, da man feststellen kann ob an gegebener pos der scanner bei 0 ist
        # => mit pos%steps_to_zero_pos == 0 -> wieder ausgangsposi
        # pos%steps_to_zero_pos -> 0..steps_to_zero_pos//2 -> man geht noch vorwärts andernfalls wieder zurück
        steps_to_zero_pos = (len(l)-1)*2
        # print(steps_to_zero_pos, steps_to_zero_pos//2, pos%steps_to_zero_pos)
        scanner = scanner + 1 if (pos % steps_to_zero_pos) < (steps_to_zero_pos//2) else scanner -1
        l[scanner] = "S"
print("part1:",severity)

severity = 0
# old: for getting max of list of first value of tuples [(depth, [S,None,None]), (depth, [..
# get every first val of tuples in inp with map(lambda x: x[0], inp) -> then get max
# map(lambda x: x[0], inp)


def adv_scanners(pos, fwall_lists):
    # advance scanner in all layers
    for l in fwall_lists:
        scanner = l.index("S")
        # print(l)
        l[scanner] = None
        # scanner gehen vor und zurück, immer einen step -> pos==anzahl schritte
        # schritte bis wieder an 0 position = (len(l)-1)*2 oder range*2-2
        # => dadurch muss man die liste gar nicht programmieren, da man feststellen kann ob an gegebener pos der scanner bei 0 ist
        # => mit pos%steps_to_zero_pos == 0 -> wieder ausgangsposi
        # pos%steps_to_zero_pos -> 0..steps_to_zero_pos//2 -> man geht noch vorwärts andernfalls wieder zurück
        steps_to_zero_pos = (len(l)-1)*2
        # print(scanner, steps_to_zero_pos)
        # print(((pos % steps_to_zero_pos) < (steps_to_zero_pos//2)))
        # print(pos)
        # print(steps_to_zero_pos, steps_to_zero_pos//2, pos%steps_to_zero_pos)
        scanner = scanner + 1 if ((pos % steps_to_zero_pos) < (steps_to_zero_pos//2)) else scanner -1
        # print(l, scanner)
        l[scanner] = "S"
# not working or at least taking way too long to reach 3875838 delay
delay = 0
caught = True
while caught:
    inp_cp = copy.deepcopy(inp)
    caught = False
    # simulate delay amount of steps
    for pos_delay in range(delay):
        adv_scanners(pos_delay, inp_cp.values())
    for pos in range(delay,max(inp_cp)+1+delay):
        # to be caught Scanner has to be at pos[0] b4 we move scanners
        try:
            caught = inp_cp[pos-delay][0] == "S"
            if caught:
                break
        except KeyError:
            pass
        adv_scanners(pos, inp_cp.values())
    # only inc delay if caught otherwise wrong value printed
    if caught:
        delay += 1
print("part2:", delay)

# solution without simulating list
# here it would be better to just have pairings of depth, range instead of depth, list of length(range)
def sim_steps(delay,find_min_delay=False):
    caught = False
    severity = 0
    for pos, l in inp.items():
        steps_to_zero_pos = (len(l)-1)*2
        # for part2 we only need to add delay to pos, since the scanners already moved delay amount of steps b4 we started
        if ((pos + delay) % steps_to_zero_pos) == 0:
            caught = True
            if find_min_delay:
                break
            severity += (pos * len(l))
    return caught, severity

print("part1:", sim_steps(0)[1])
delay = 0
# keep +1 delay while caught remains True
while sim_steps(delay, find_min_delay=True)[0]:
    delay += 1
print("part2:", delay)

    
