from d1 import readfile

inp = readfile("d2_input.txt")
# this will just give one list with ints [int(s) for row in inp.splitlines() for s in row.split()]
# use NESTED list compr (or map, but list compr also faster than map)
inp = [sorted([int(s) for s in row.split()]) for row in inp.splitlines()]
# task is to determine diff between largest and smallest in row -> sorted list last-first for each row -> sum differences
# part1: nsum = sum([l[-1] - l[0] for l in inp])

nsum = 0
# task: find the only two numbers in each row where one evenly divides the other, find those numbers on each line, divide them, and add up each line's result
for row in inp:
    found = False
    # every number in a row
    for n in row:
        if found:
            break
        # divide every nr (but itself) in the row by n -> even divide (modulo 0)
        for d in row:
            if n == d:
                continue
            elif d % n == 0:
                nsum += d//n
                found = True
                break
    else:
        # else suite is executed after the for, but only if the for terminates normally (not by a break)
        # -> no number found
        print("No number found!")


l = list("ABC")

# listed in the documentation of itertools.permutations
def permutations_doc(iterable, r=None):
    # permutations('ABCD', 2) --> AB AC AD BA BC BD CA CB CD DA DB DC
    # permutations(range(3)) --> 012 021 102 120 201 210
    pool = tuple(iterable)
    n = len(pool)
    r = n if r is None else r
    if r > n:
        return
    indices = range(n)
    cycles = range(n, n-r, -1)
    yield tuple(pool[i] for i in indices[:r])
    while n:
        for i in reversed(range(r)):
            cycles[i] -= 1
            if cycles[i] == 0:
                indices[i:] = indices[i+1:] + indices[i:i+1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                yield tuple(pool[i] for i in indices[:r])
                break
        else:
            return
# print(permutations(l))

#print(list(gen_perm(inp[0])))

print(nsum)
