start = 130254
end = 678275

import time
t_start = time.time()
valid = 0
for i in range(start, end + 1):
    i_str = str(i)
    double_adjacent = False
    before = i_str[0]
    for c in i_str[1:]:
        if int(c) < int(before):
            double_adjacent = False
            break
        elif c == before:
            double_adjacent = True
        before = c

    if double_adjacent:
        valid += 1
# short but slower (run time 1.3s vs 3.6s, but way faster in programmer time) part1
# valid = 0
# for i in range(start, end + 1):
#     digits = [int(d) for d in str(i)]
#     decreasing = any(digits[i] > digits[i+1] for i in range(len(digits) - 1))
#     double_adjacent = any(digits[i - 1] == digits[i] for i in range(1, len(digits)))

#     if double_adjacent and not decreasing:
#         valid += 1
t_stop = time.time()
dt = t_stop - t_start
print(dt)
print("PART1:", valid)

# PART2

valid = 0
for i in range(start, end + 1):
    i_str = str(i)
    valid_pw = False
    double_adjacent = False
    adjacent_d = None
    for si, c in enumerate(i_str):
        if si == 0:
            continue
        before = i_str[si - 1]
        if int(c) < int(before):
            valid_pw = False
            break
        # save current digit of adjacents so we don't count adjacent digits that
        # are part of a larger group than 2 adjacents
        if not valid_pw and adjacent_d != c and c == before:
            double_adjacent = True
            adjacent_d = c

        # check if next digits is not the same as the double adjacent digits
        # or if there's no next digit
        if (double_adjacent and ((si+1 < len(i_str) and i_str[si+1] != c) or (si+1 == len(i_str)))):
            valid_pw = True
        else:
            double_adjacent = False

    valid += valid_pw
print("PART2:", valid)
