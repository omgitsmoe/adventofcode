from collections import Counter
from utils import readfile

inp = readfile("d2_input.txt")


def cmp_str(x, y):
    if len(x) != len(y):
        return False
    ignored = False
    for i, c in enumerate(x):
        if c != y[i]:
            # ignore one wrong char
            if ignored:
                return False
            else:
                ignored = True
    return True


two = 0
three = 0
ids = []
for ln in inp.splitlines():
    letters = Counter(ln.strip())
    counts = letters.values()
    # box ids can only count once for two or three
    if 2 in counts:
        two += 1
    # and they can count for both
    if 3 in counts:
        three += 1
    # part2
    for id_, cnter in ids:
        # counter1 - counter2 subtracts counts only keeping positive ones
        diff = cnter - letters
        # at most one letter diff
        if sum(diff.values()) == 1:
            # compare str ignoring one diff char
            if cmp_str(ln, id_):
                # print common chars by checking if other str has the same char at that pos
                print("Part2:", "".join(c for i, c in enumerate(ln) if c == id_[i]))
    ids.append((ln.strip(), letters))

print("Part1:", two*three)
