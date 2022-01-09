# -*- coding: utf-8 -*-
def readfile(fname):
    with open(fname, "r", encoding="UTF-8") as f:
        return f.read()

def redistribute(l, index):
    # blocks to redistribute
    to_redistribute = l[index]
    l[index] = 0
    cur_ind = index
    while to_redistribute > 0:
        cur_ind += 1
        # limit index to 0..len(l)-1
        cur_ind = cur_ind % len(l)
        # print(l[cur_ind])
        # one block on current reomving one from redistribute
        l[cur_ind] += 1
        # print(l[cur_ind], to_redistribute)
        to_redistribute -= 1
        # print(to_redistribute)


inp = readfile("d6_input.txt")
inp = [int(s) for s in inp.split()]
# inp = [0,2,7,0]

def p1():
    n = 0
    states = set()
    # CAREFUL! frozenset removes duplicates so not usable here -> use tuple (hashable, list isnt)
    states.add(tuple(inp))
    while True:
        n += 1
        # find max, then redistribute by removing all blocks from max and then walking over all banks adding one from the blocks to redistribute
        to_redistribute_ind = inp.index(max(inp))
        # print(to_redistribute_ind, inp)
        redistribute(inp, to_redistribute_ind)
        # print(inp)
        # newstate = tuple(inp)
        # instead of checking if newstate is alrdy in states we could add it (directly, without creating inbeteween var newstate) to states and if len(states) didnt increase -> break
        # only slightly faster: is in: 2.515483873261722 len check: 2.4816886584815157
        # if newstate in states:
        #    break
        # states.add(newstate)
        len_before = len(states)
        states.add(tuple(inp))
        if len(states) == len_before:
            break
    print(n)
p1()

def p2():
    n = 0
    # starting from end state from p1 when do we reach that same state again?
    # state to reach
    end_state = inp.copy()
    while True:
        n += 1
        # find max, then redistribute by removing all blocks from max and then walking over all banks adding one from the blocks to redistribute
        to_redistribute_ind = inp.index(max(inp))
        # print(to_redistribute_ind, inp)
        redistribute(inp, to_redistribute_ind)
        # current matching state to reach?
        if inp == end_state:
           break
    print(n)
p2()



