with open("d10_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read().strip()

inp_p1 = [int(x) for x in inp.split(",")]
# use ord to get int from ascii code char
inp_p2 = [ord(c) for c in inp] 
# add fix suffix
inp_p2.extend([17, 31, 73, 47, 23])
print(inp_p2, type(inp_p2[0]))
def p1():
    l = list(range(0, 256)) #[0, 1, 2, 3, 4]#
    l_len = len(l)
    pos = 0
    skip = 0

    for length in inp_p1: #[3, 4, 1, 5]:#
        # Python allows you to assign to slices
        # from docs: Slicings may be used as expressions or as targets in assignment or del statements
        # thus it's possible to do everything on one line:
        # foo[x:y] = foo[y-1:x-1:-1] # reverse from x to y
        # Note that foo[y-1:x-1:-1] has the same meaning as foo[x:y][::-1]
        # reverse has to wrap in our example -> easier? to use loop instead of slicing

        if length > len(l):
            continue  # length invalid
        # REVERSING
        # length//2 == nr of replacements to reverse list
        for i in range(length//2):
            idx_end = (pos + length-1 - i) % l_len  # wrap index
            idx_repl = (pos + i) % l_len  # wrap index
            if idx_end == idx_repl:
                # element in middle
                continue
            temp = l[idx_repl]
            l[idx_repl] = l[idx_end]
            l[idx_end] = temp
            #print(idx_repl, idx_end, l)
        pos += length + skip
        skip += 1
    print("part1:", l[0]*l[1])
p1()

def p2():
    l = list(range(0, 256)) #
    l_len = len(l)
    # pos, skip and length sequence is preserved between rounds
    pos = 0
    skip = 0

    for n in range(64):
        for length in inp_p2:
            # ARGH forgot to change len(inp_p2) to len(l) so almost every length was invalid
            # and it isnt even needed here since the ascii values only go from 0-255
            if length > len(l):
                continue  # length invalid
            # REVERSING
            # length//2 == nr of replacements to reverse list
            for i in range(length//2):
                idx_end = (pos + length-1 - i) % l_len  # wrap index
                idx_repl = (pos + i) % l_len  # wrap index
                if idx_end == idx_repl:
                    # element in middle
                    continue
                temp = l[idx_repl]
                l[idx_repl] = l[idx_end]
                l[idx_end] = temp
                #print(idx_repl, idx_end, l)
            pos += length + skip
            skip += 1
    # could use reduce for this:
    # print reduce(lambda i, j: int(i) ^ int(j), bit)
    # reduce(...) reduce(function, sequence[, initial]) -> value
    # Apply a function of two arguments cumulatively to the items of a sequence, from left to right, so as to reduce the sequence to a single value. For example, reduce(lambda x, y: x+y, [1, 2, 3, 4, 5]) calculates ((((1+2)+3)+4)+5). If initial is present, it is placed before the items of the sequence in the calculation, and serves as a default when the sequence is empty.
    # build dense hash by XORing (^ in python) each number in blocks of 16
    dense = []
    # None and check if set isnt needed since 0^5^3 == 5^3 -> just use 0 without if check
    # cur_hash = None; if cur_hash: cur_hash ^= l[i]; else: cur_hash = l[i]...
    cur_hash = 0
    for i in range(len(l)):
        cur_hash ^= l[i]
        # 16 blocks of 16 in list of 256 (0-255) numbers
        # WRONG!!! end of block reached i%15==0 and start of loop excluded (i!=0); start of next block i%16==0
        # -> at start 16nrs(0-15), but 2nd block only 15nrs(16-30)
        # end of block -> i%16 == 15
        if (i%16) == 15:
            # print(i, cur_hash)
            dense.append(cur_hash)
            cur_hash = 0
        #print(dense, i, i%15, l[i], cur_hash)
    # using format: '{0:02x}'.format(a) or hex(int) but hex diff formatting
    dense_hex = ['{0:02x}'.format(n) for n in dense]
    print(len(dense), dense, dense_hex)
    print("part2:", "".join(dense_hex))


p2()


