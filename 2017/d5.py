# -*- coding: utf-8 -*-
def readfile(fname):
    with open(fname, "r", encoding="UTF-8") as f:
        return f.read()

inp = readfile("d5_input.txt")
inp = [int(ln) for ln in inp.splitlines()]
# inp = [0, 3, 0, 1, -3]

def p1():
    n = 0
    current = 0
    cur_ind = 0
    while True:
        # print("current index:", cur_ind)
        # pythonic would be using try..except IndexError
        if cur_ind > len(inp)-1:
            break
        current = inp[cur_ind]
        # advance current steps
        next_i = cur_ind + current
        # increment current instruction
        inp[cur_ind] = current + 1
        # print(f"current {current} next_i {next_i} old_new {inp[cur_ind]}")
        cur_ind = next_i
        n += 1
    print(n)


# could avoid using sep var for current and do it all using list[index]
# same for next_i
def p2():
    n = 0
    current = 0
    cur_ind = 0
    while True:
        # print("current index:", cur_ind)
        # pythonic would be using try..except IndexError
        if cur_ind > len(inp)-1:
            break
        current = inp[cur_ind]
        # advance current steps
        next_i = cur_ind + current
        # increment current instruction
        if current >= 3:
            inp[cur_ind] = current - 1
        else:
            inp[cur_ind] = current + 1
        # print(f"current {current} next_i {next_i} old_new {inp[cur_ind]}")
        cur_ind = next_i
        n += 1
    print(n)

p2()
