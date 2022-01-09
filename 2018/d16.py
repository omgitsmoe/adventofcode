from utils import readfile

inp = readfile("d16.in").splitlines()

reg = [0 for _ in range(4)]
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
opcode_nrs = [[] for _ in range(16)]
samples = []

i = 0
while True:
    # break part1 on >1 newline
    if inp[i] == "" and inp[i+1] == "":
        break
    b4 = [int(d) for d in inp[i][9:-1].split(", ")]
    # instruction: opcode input1 input2 output
    opc_nr, a, b, out = [int(d) for d in inp[i+1].split(" ")]
    after = [int(d) for d in inp[i+2][9:-1].split(", ")]
    match = []
    for opc_name, opc in opcodes.items():
        reg = b4.copy()
        # check if opcode result matches register out/c
        if opc(a, b) == after[out]:
            match.append(opc_name)
    # tuple of opc_nr, list of names (just needed for prt2
    samples.append((opc_nr, match))
    i += 4

print("Part1:", sum(1 for matches in samples if len(matches[1]) >= 3))

while samples:
    last_opc_nr = None
    last_opc = None
    for smpl in samples:
        # for part2 if we have only a sample only matching one opcode
        # -> assign that opc_nr the opc_name
        # or better assign it the lambda for the instruction from opcodes directly
        if len(smpl[1]) == 1:
            last_opc_nr = smpl[0]
            last_opc = smpl[1][0]
            opcode_nrs[last_opc_nr] = opcodes[last_opc]
            break
    # remove all samples of that opc_nr and remove the opc from other opc_nrs
    if last_opc_nr is not None:
        new_samples = []
        for (opc_nr, opcs) in samples:
            # filter last_opc_nr
            if opc_nr == last_opc_nr:
                continue
            # remove opc from samples of other nrs
            opcs = [opc for opc in opcs if opc != last_opc]
            new_samples.append((opc_nr, opcs))
        samples = new_samples
    else:
        print(samples)
        raise Exception("No single match!")

reg = [0 for _ in range(4)]
# continue 2 lines down from line nr from part1
i += 2
while i < len(inp):
    opc, a, b, c = [int(d) for d in inp[i].split(" ")]
    reg[c] = opcode_nrs[opc](a, b)
    i += 1
print("Part2:", reg[0])
