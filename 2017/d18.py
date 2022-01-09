import collections
with open("d18_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read().strip()

# inp ="""set a 1
# add a 2
# mul a a
# mod a 5
# snd a
# set a 0
# rcv a
# jgz a -1
# set a 1
# jgz a -2"""
# inp = """snd 1
# snd 2
# snd p
# rcv a
# rcv b
# rcv c
# rcv d"""
inp = [ln.split() for ln in inp.splitlines()]

def reg_or_value(x, regdict):
    try:
        return int(x)
    except ValueError:
        return regdict[x]

# use defaultdict(int) with value 0 or instead of literal dict[key] use get mehtode with default if key doesnt exist dict.get(key, default)
regs = collections.defaultdict(int)
last_snd = None
rec = None

asm_i = 0
# interval comparison works in python
while 0 <= asm_i < len(inp):
    asm = inp[asm_i]
    # print(regs)
    # print(asm)
    if asm[0] == "snd":
        last_snd = regs[asm[1]]
    elif asm[0] == "set":
        # asm X (Y), Y can be either int or name of register -> except ValueError -> treated asm[2] as number but it was a register name
        try:
            # set X to Y
            regs[asm[1]] = int(asm[2])
        except ValueError:
            regs[asm[1]] = regs[asm[2]]
    elif asm[0] == "add":
        # all the try and except could be handled in a function instead reg_or_val(x): try: return int(x) except ValueError: return regs[...
        try:
            # add Y to X
            regs[asm[1]] += int(asm[2])
        except ValueError:
            regs[asm[1]] += regs[asm[2]]
    elif asm[0] == "mul":
        try:
            # set X to X*Y
            regs[asm[1]] *= int(asm[2])
        except ValueError:
            regs[asm[1]] *= regs[asm[2]]
    elif asm[0] == "mod":
        try:
            # set X to X%Y
            regs[asm[1]] %= int(asm[2])
        except ValueError:
            regs[asm[1]] %= regs[asm[2]]
    elif asm[0] == "rcv":
        if regs[asm[1]] != 0:
            # part1 value of last sound when rcv is executed for the first stime and asm[1] != 0
            rec = last_snd
            break
    elif asm[0] == "jgz":
        # WARNIG was my bug in part2 since X here can also be a value instead of a register and i dint check for that since i looked in my input and didnt see any number as X (i misstook a 1 for l)
        # created function that returns converted value or value of register -> less code and cleaner
        if reg_or_value(asm[1], regs) > 0:
            try:
                # jump Y steps if X > 0
                asm_i += int(asm[2])
            except ValueError:
                asm_i += regs[asm[2]]
            # we directly continue with asm we jumped to
            continue
    asm_i += 1
print("part1:", rec)

idx_list = [0, 0]
p0_sent = []
p0_regs = collections.defaultdict(int)
p1_sent = []
p1_regs = collections.defaultdict(int)
p1_regs["p"] = 1
sent_sum = [0, 0]
waiting = [0, 0]
def run_prog(i, regdict, sndl, rcvl):
    # i -> idx_list[i] asm_i of program with id i -> value in list is mutable, int isnt
    # only advance one step with this func
    # interval comparison works in python
    if 0 <= idx_list[i] < len(inp):
        asm = inp[idx_list[i]]
        # print("PROG",i, idx_list[i],asm)
        # print(regdict)
        # print(sndl, rcvl)
        # print(sent_sum)
        if asm[0] == "snd":
            # send value of X to other prog/sendlist
            sndl.append(reg_or_value(asm[1], regdict))
            sent_sum[i] += 1
        elif asm[0] == "set":
            # asm X (Y), Y can be either int or name of register -> except ValueError -> treated asm[2] as number but it was a register name
            # set X to Y
            regdict[asm[1]] = reg_or_value(asm[2], regdict)
        elif asm[0] == "add":
            # add Y to X
            regdict[asm[1]] += reg_or_value(asm[2], regdict)
        elif asm[0] == "mul":
            # set X to X*Y
            regdict[asm[1]] *= reg_or_value(asm[2], regdict)
        elif asm[0] == "mod":
            # set X to X%Y
            regdict[asm[1]] %= reg_or_value(asm[2], regdict)
        elif asm[0] == "rcv":
            # set X to next value in line in rcvl(recieve list)
            # If no values are in the queue, the program waits for a value to be sent to it. Programs do not continue to the next instruction until they have received a value.
            try:
                regdict[asm[1]] = rcvl.pop(0)
            except IndexError:
                # pop leads to IndexError -> list empty
                # not 0 -> True -> True is evaluated as 1 in python; not (1 or nonzero-number) -> False -> 0
                if waiting[not i]:
                    # both progs would be waiting -> deadlock
                    return False
                else:
                    # wait till we can recieve value
                    waiting[i] = True
                    return True
            else:  # executed if the try clause does not raise an exception
                # recieved successfully
                waiting[i] = False
        elif asm[0] == "jgz":
            # WARNIG was my bug in part2 since X here can also be a value instead of a register and i dint check for that since i looked in my input and didnt see any number as X (i misstook a 1 for l)
            # created function that returns converted value or value of register -> less code and cleaner
            if reg_or_value(asm[1], regdict) > 0:
                # jump Y steps if X > 0
                idx_list[i] += reg_or_value(asm[2], regdict)
                # we directly continue with asm we jumped to
                return True
        idx_list[i] += 1
        # normal exec
        return True
    else:
        # idx out of bounds
        return False

# advance each prog one step till one of them returns False
while run_prog(0, p0_regs, p0_sent, p1_sent) and run_prog(1, p1_regs, p1_sent, p0_sent):
    pass
print("part2:", sent_sum[1])
# It should be noted that it would be equally valid for the programs to run at different speeds; for example, program 0 might have sent all three values and then stopped at the first rcv before program 1 executed even its first instruction.

