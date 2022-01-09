with open("d8_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

inp = inp.splitlines()
# split in operation and condition
inp = [ln.split(" if ") for ln in inp]

registers = {}

# hack would be to replace inc and dec with + and - and add ternary else 0
# and use exec or eval to execute it
# -> but unsafe
# e.g.
# from aocd import data
# from collections import defaultdict
# 
# d = defaultdict(int)
# data += '\n'
# data = data.replace('\n', ' else 0\n')
# data = data.replace('inc', '+=')
# data = data.replace('dec', '-=')
# exec(data, {}, d)
# print(max(d.values()))
# ----OR----
# from collections import defaultdict
# 
# lines = open('input.txt').read().splitlines()
# regs = defaultdict(int)
# mxv = 0
# for line in lines:
#     reg, inst, num, iff, regc, op, num2 = line.split()
#     if eval("regs[regc] " + op + num2):
#         if inst == 'inc':
#             regs[reg] += int(num)
#         else:
#             regs[reg] -= int(num)
#         mxv = max(mxv, regs[reg])
# 
# print max(regs.values()) # PART 1
# print mxv # PART 2
# -----best solution from thread in py-----
# import operator as op
# from collections import defaultdict
# comps = {'>': op.gt, '<': op.lt, '>=': op.ge, '<=': op.le, '!=': op.ne, '==': op.eq}
# 
# def solve(input, mx=float('-inf'), ops=dict(inc=1, dec=-1)):
#     regs = defaultdict(int)
#     for r1, op, v1, _, r2, c, v2 in [l.split() for l in input.splitlines() if l]:
#         regs[r1] += ops[op] * int(v1) if comps[c](regs[r2], int(v2)) else 0
#         mx = max(mx, regs[r1])
#     return max(regs.values()), mx
# 
# part1, part2 = solve(input)
def p12():
    highest = 0
    for ln in inp:
        # is there a simpler way? e.g. eval (-> unsafe)
        if check_condition(ln[1]):
            var, operation, nr = ln[0].split()
            if operation == "inc":
                # account for register key not being in registers
                # could be avoided with defaultdict or dict.get(key, default)
                try:
                    registers[var] += int(nr)
                except KeyError:
                    registers[var] = 0 + int(nr)
            else:
                try:
                    registers[var] -= int(nr)
                except KeyError:
                    registers[var] = 0 - int(nr)
            # part2
            if registers[var] > highest:
                highest = registers[var]
    print(max(registers.values()), highest)

def check_condition(cond_str):
    var, cond, nr = cond_str.split()
    nr = int(nr)
    # do exception handling here instead of separately at every return for every cond
    try:
        reg_val = registers[var]
    except KeyError:
        registers[var] = 0
        reg_val = 0
    if cond == ">":
        return reg_val > nr
    elif cond == "<":
        return reg_val < nr
    elif cond == "==":
        return reg_val == nr
    elif cond == ">=":
        return reg_val >= nr
    elif cond == "<=":
        return reg_val <= nr
    elif cond == "!=":
        return reg_val != nr

p12()
