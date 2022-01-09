from utils import readfile

inp = readfile("d19.in").strip().splitlines()
# inp = """#ip 0
# seti 5 0 1
# seti 6 0 2
# addi 0 1 0
# addr 1 2 3
# setr 1 0 0
# seti 8 0 4
# seti 9 0 5""".splitlines()

reg = [0 for _ in range(6)]
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
ip = 0
ip_reg = int(inp.pop(0).split(" ")[1])


# we halt when instruction pointer(ip) points to invalid loc
while 0 <= ip < len(inp):
    break  # remove for part1
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
    # print(f"ip={ip} {reg}")
    # print("==========")
print("Part1:", reg[0])

# part2 (if starting from instruction 0)
# reg[0] = 1
# reduced setup instructions to this manually
# ip = 1
# reg = [0, 0, 365120, 0, 366078, 0]
# reduced the instructions to python code and then simplified it
# reg[5] = 1
# while reg[5] <= reg[4]:
#     reg[3] = 1
#     while reg[3] <= reg[4]:
#         reg[2] = reg[5] * reg[3]
#         if reg[2] == reg[4]:
#             reg[0] = reg[5] + reg[0]
#         reg[3] += 1
#     reg[5] += 1
# print(reg[0])

ip = 1
reg = [0, 0, 10550400, 0, 10551358, 0]
reg[5] = 1
while reg[5] <= reg[4]:
    # reg[3] is counted up from 1 till it reaches reg[4]
    # if there is a nr that == reg[4] when multiplied with reg[5]
    # reg[5] is added to reg[0]
    # theoretically need to check (at the start) if that nr is > reg[3]
    # but since reg[5] is 1 at the start it doesnt matter
    if (reg[4] % reg[5]) == 0:
        reg[0] += reg[5]
    reg[5] += 1
print("Part2:", reg[0])
