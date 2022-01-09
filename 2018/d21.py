from utils import readfile

inp = readfile("d21.in").strip().splitlines()

opcodes = {
        "addr": lambda a, b: reg[a] + reg[b],
        "addi": lambda a, b: reg[a] + b,
        "mulr": lambda a, b: reg[a] * reg[b],
        "muli": lambda a, b: reg[a] * b,
        "banr": lambda a, b: reg[a] & reg[b],
        "bani": lambda a, b: reg[a] & b,
        "borr": lambda a, b: reg[a] | reg[b],
        "bori": lambda a, b: reg[a] | b,
        "setr": lambda a, b: reg[a],
        "seti": lambda a, b: a,
        "gtir": lambda a, b: 1 if a > reg[b] else 0,
        "gtri": lambda a, b: 1 if reg[a] > b else 0,
        "gtrr": lambda a, b: 1 if reg[a] > reg[b] else 0,
        "eqir": lambda a, b: 1 if a == reg[b] else 0,
        "eqri": lambda a, b: 1 if reg[a] == b else 0,
        "eqrr": lambda a, b: 1 if reg[a] == reg[b] else 0,
        }
ip_reg = int(inp.pop(0).split(" ")[1])

# reg0 is only used in l29 eqrr 3 0 5 -> if reg3 == reg0 write 1 to reg5
# which then will lead to exiting the program
# print out whats in reg3 at l29 and set reg0 to that (since it doesnt get used otherwise)
reg = [0 for _ in range(6)]
ip = 0
# we halt when instruction pointer(ip) points to invalid loc
while 0 <= ip < len(inp):
    # part1
    if ip == 29:
        # print out reg3 so we know what value we need in reg0 for the comparison later
        print("Part1:", reg[3])
        break
    ln = inp[ip].split(" ")
    # print(f"ip={ip} {reg} {ln}")
    # store instruction pointer in reg before executing instructions
    reg[ip_reg] = ip

    opc = ln[0]
    a, b, c = [int(d) for d in ln[1:]]
    reg[c] = opcodes[opc](a, b)

    # set ip to value of reg we stored it in (in case it was modified)
    # and add one
    ip = reg[ip_reg] + 1


# parsing the lines here instead of everytime in the instruction code
# to gain performance -> (not measured but its ~10x faster)
parsed = []
for ln in inp:
    lnl = ln.split(" ")
    opc = lnl[0]
    a, b, c = [int(d) for d in lnl[1:]]
    parsed.append((opc, a, b, c))

r = [0 for _ in range(6)]
seen = set()
r[1] = 65536
r[3] = 10373714
while True:
    # vv same as r[1] % 256
    r[5] = r[1] & 255  # l8
    r[3] += r[5]
    # vv same as %= 16777216
    # r[3] &= 16777215
    # r[3] *= 65899
    # r[3] &= 16777215
    r[3] = ((r[3] & 16777215) * 65899) & 16777215
    if r[1] < 256:
        # if r[3] == r[0]:
        #    break
        # goto l6
        r[1] = r[3] | 65536  # l6
        # r[3] changes only by r[5] and static values
        # and r5 is masked by 255 and depends on r1
        # so check when r1 repeats -> then the value for r3
        # is the value for r0 to reach the most instructions
        if r[1] in seen:
            # could/should also check for r[3] being unique since if we have
            # seen it before the program would have terminated (since wed
            # use it as value for r0)
            print("Part2:", r[3])
            break
        seen.add(r[1])
        r[3] = 10373714
        continue
    # while r[1] < (256 * (r[5] + 1)):
    #     r[5] += 1
    # r[1] = r[5]
    # -> above is summarized to integer division of r[1] by 256
    r[1] = r[1]//256
    # goto l8

# bruteforcing vv
# reg = [0 for _ in range(6)]
# ip = 0
# prev_reg3 = 0
# seen_r3d = set()
# l29_counter = 0
# while 0 <= ip < len(parsed):
#     # part1
#     if ip == 29:
#         # print out reg3 so we know what value we need in reg0 for the comparison later
#         l29_counter += 1
#         diff = reg[3]-prev_reg3
#         print(l29_counter, "reg3:", reg[3], "diff", diff, diff in seen_r3d)
#         seen_r3d.add(diff)
#         prev_reg3 = reg[3]
#     reg[ip_reg] = ip

#     opc, a, b, c = parsed[ip]
#     reg[c] = opcodes[opc](a, b)

#     ip = reg[ip_reg] + 1

# find cycle for reg3
# bruteforce of finding cycle took 3 mins with pypy would have taken forever without
# ith time 29 reg3     seen       diff     seen
# 10496 reg3: 15155337 False diff 13320048 False
# 10497 reg3: 16477902 False diff 1322565 False  => biggest value for reg3 b4 it starts repeating
# 10498 reg3: 3107527 False diff -13370375 False
# 10499 reg3: 14694490 False diff 11586963 True
# 10500 reg3: 5299527 False diff -9394963 True
# 10501 reg3: 10319915 False diff 5020388 True
# part2 -> 16477902
