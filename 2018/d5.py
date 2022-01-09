from utils import readfile

inp = readfile("d5_input.txt").strip()
inp = list(inp)

i = 0
while True:
    if i == len(inp)-2:
        # end of list
        break
    elif abs(ord(inp[i]) - ord(inp[i+1])) == 32:
        # same polymer type (same char) of diff polarity (upper vs lowercase) are next to each other

        # use slice to delete multiple contiguous elements from list
        del inp[i:i+2]
        # since we delete the element at i this wont delete the original i+1 but i+2
        # del inp[i]
        # del inp[i+1]
        if i > 0:
            i += -1
    else:
        i += 1
print("Part1:", len(inp))

lengths = []
for c in list("abcdefghijklmnopqrstuvwxyz"):
    # replace one char/polymer type in both cases/polarity
    inp_repl = "".join(inp).replace(c, "").replace(c.upper(), "")
    inp_repl = list(inp_repl)
    i = 0
    while True:
        if i == len(inp_repl)-2:
            break
        elif abs(ord(inp_repl[i]) - ord(inp_repl[i+1])) == 32:
            # use slice to delete multiple contiguous elements from list
            del inp_repl[i:i+2]
            if i > 0:
                i += -1
        else:
            i += 1
    lengths.append((c, len(inp_repl)))
print("Part2:", min(lengths, key=lambda l: l[1]))
