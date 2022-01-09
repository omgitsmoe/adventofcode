
f = open("d05.in")
codes_o = [int(x) for x in f.read().split(",")]
f.close()

codes_o = [int(x) for x in "1002,4,3,4,33".split(",")]
codes = [x for x in codes_o]

def get_param(pmodes, pos, pval):
    try:
        pmode = pmodes[pos]
    except IndexError:
        pmode = "0"
    print(pos, pmode, pval)
    if pmode == "0":
        print('reg')
        return codes[pval]
    else:
        print('val')
        return pval

# parameters that an instruction writes to will never be in immediate mode
def add(x, y, out):
    codes[out] = x + y

def mult(x, y, out):
    codes[out] = x * y

def mov(x):
    codes[x] = x

def out(x):
    return codes[x]

opcodes = {
        # nparams, func
        1: (3, add),
        2: (3, mult),
        3: (1, mov),
        4: (1, out),
        }

i = 0
last_out = None
func = None
while True:
    instr = codes[i]
    print(codes, instr)
    # test last out for err or halt which would mean end of program
    if last_out is not None and func is out and last_out != 0:
        # immediately followed by halt -> diagnostic code at end of program
        if instr == 99:
            break
        else:
            raise Exception("func", str(func), "returned non-zero")
    # pmodes are right-to-left, so reverse the list
    if instr > 99:
        instr = [x for x in str(codes[i])]
        pmodes, opcode = instr[:-2][::-1], int("".join(instr[-2:]))
    else:
        pmodes, opcode = [], int(instr)

    nparams, func = opcodes[opcode]
    print('pmodes', pmodes)
    params = [get_param(pmodes, p_idx, codes[i + 1 + p_idx]) for p_idx in range(nparams - 1)] + [codes[i + nparams]]
    print('params', params)
    last_out = func(*params)
    i += 1 + nparams
print("PART1:", last_out)
