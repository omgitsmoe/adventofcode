from utils import readfile

inp = readfile("d1_input.txt")

freq = 0
for ln in inp.splitlines():
    nr = int(ln.strip())
    freq += nr

print("Part1:", freq)

seen = {0}
freq = 0
found = False
while not found:
    for ln in inp.splitlines():
        nr = int(ln.strip())
        freq += nr
        if freq in seen:
            print("Part2:", freq)
            found = True
            break
        else:
            seen.add(freq)
