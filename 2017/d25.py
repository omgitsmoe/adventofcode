from collections import defaultdict
tape = defaultdict(int)
states = {"A": ((1, 1, "B"), (0, -1, "D")),
          "B": ((1, 1, "C"), (0, 1, "F")),
          "C": ((1, -1, "C"), (1, -1, "A")),
          "D": ((0, -1, "E"), (1, 1, "A")),
          "E": ((1, -1, "A"), (0, 1, "B")),
          "F": ((0, 1, "C"), (0, 1, "E"))}
cur_i = 0

step = 0
state = "A"
while step < 12302209:
    cur_val = tape[cur_i]
    # val is 0 or 1 -> use as index for tuple to get values for new val move state
    newval, move, newstate = states[state][cur_val]
    tape[cur_i] = newval
    state = newstate
    cur_i += move
    step += 1

# diagnositc checksum
print(sum(tape.values()))
