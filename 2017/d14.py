inp = "stpzcrnm"#"flqrgnkx"#"stpzcrnm"

def knot_hash(string):
    inp_p2 = [ord(c) for c in string] + [17, 31, 73, 47, 23]

    l = list(range(0, 256)) #
    l_len = len(l)
    # pos, skip and length sequence is preserved between rounds
    pos = 0
    skip = 0

    for n in range(64):
        for length in inp_p2:
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
                # without using temp var
                l[idx_repl], l[idx_end] = l[idx_end], l[idx_repl]
            pos += length + skip
            skip += 1
    dense = []
    # None and check if set isnt needed since 0^5^3 == 5^3 -> just use 0 without if check
    cur_hash = 0
    for i in range(len(l)):
        cur_hash ^= l[i]
        # 16 blocks of 16 in list of 256 (0-255) numbers
        # end of block -> i%16 == 15
        if (i%16) == 15:
            dense.append(cur_hash)
            cur_hash = 0
    # using format: '{0:02x}'.format(a) or hex(int) but hex diff formatting
    dense_hex = ['{0:02x}'.format(n) for n in dense]
    return "".join(dense_hex)

used = 0
# y, x
grid = []
for n in range(128):
    hasinphstr = f"{inp}-{n}"
    hashstr = knot_hash(hasinphstr)
    # turn each hexadecimal digit to its equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110
    bin_str = "".join((bin(int(d, 16))[2:].zfill(4) for d in hashstr))
    # intlist is row in grid (row==Zeile, column==Spalte)
    bin_ints = [int(d) for d in bin_str]
    grid.append(bin_ints)
    used += bin_str.count("1")

print("part1:", used)

def get_neighbours(pos, seen):
    # return neighbours pos if theyre true and not in seen
    y, x = pos
    result = []
    #               up, bot, left, right
    # WARNING y and x can become negative this way and python accepts those as indexes -> no IndexError
    # -> check for ny,nx being neg.
    for ny, nx in [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]:
        # only return not seen
        if (ny, nx) not in seen and (ny >= 0 and nx >= 0):
            try:
                # only return filled(1->True) neighbours
                if grid[ny][nx]:
                    result.append((ny, nx))
                # can add them here since they either were alrdy in seen/are false or belong to the current region -> wont need to visit again
                seen.add((ny, nx))
            # handle looking beyond list (-> IndexError)
            except IndexError:
                pass
    return result

# grid = [[1,1,0,0,1,1,0,1], #5
#         [1,0,1,1,0,1,0,1],
#         [1,1,0,1,1,1,0,1],
#         [1,0,0,0,1,0,1,1],
#         [0,1,1,0,1,0,0,0],
#         [0,0,0,0,1,1,0,1]]
# grid is 128x128 = 16384, but seen had 17773 elements -> was not checking for neighbour indexes becoming negative, forgot theyre valid indexes in python
seen = set()
regions = 0
for y, row in enumerate(grid):
    for x in range(len(row)):
        if (y,x) in seen:
            continue
        elif not grid[y][x]:
            seen.add((y, x))
            continue
        seen.add((y, x))
        # get pos of neighbours that are true and not in seen
        neighbours = get_neighbours((y, x), seen)
        while neighbours:
            # continue searching for neighbours till list is empty
            npos = neighbours.pop()
            neighbours += get_neighbours(npos, seen)
        # region finished
        # each filled field is its own region even without filled neighbours
        regions += 1
print("part2:", regions)

