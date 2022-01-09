inp = 2866

grids = {}
# since were looking for 3x3 grids stop 2 rows/cols b4 300
for y in range(1, 300 + 1 - 2):
    for x in range(1, 300 + 1 - 2):
        power_total = 0
        for gy in range(3):
            for gx in range(3):
                rack_id = x + gx + 10
                power_lvl = rack_id * (y + gy) + inp
                power_lvl *= rack_id
                if power_lvl > 99:
                    # extract 100s-digit
                    power_lvl = (power_lvl // 100) % 10
                    power_lvl -= 5
                else:
                    power_lvl = -5
                power_total += power_lvl
        grids[(x, y)] = power_total

print("Part1:", max(grids.items(), key=lambda x: x[1]))


def pwr_lvl(y, x):
    rack_id = x + 10
    power_lvl = rack_id * y + inp
    power_lvl *= rack_id
    # extract 100s-digit
    power_lvl = (power_lvl // 100) % 10
    power_lvl -= 5
    return power_lvl


# pre-generate grid
grid = [[pwr_lvl(y+1, x+1) for x in range(300)] for y in range(300)]

# # sum from 0,0 in list (or 1,1 in example) to y, x
# # https://www.geeksforgeeks.org/submatrix-sum-queries/
# sums = []
# # first pass: calc sums of cols -> (assuming grid[0] = 0,1,2,3,4..) 0,1,3,6,10..
# for row in grid:
#     sum_row = []
#     cur_sum = 0
#     for col in row:
#         cur_sum += col
#         sum_row.append(cur_sum)
#     sums.append(sum_row)

# # second pass: add sum of col one row above so that y,x fullfills:
# # value at any point (x, y) in the summed-area table is the sum of all the
# # pixels above and to the left of (x, y), inclusive
# for y in range(1, 300):
#     for x in range(300):
#         sums[y][x] += sums[y-1][x]


# grid = [[ 1, 2, 3, 4, 5, 6],
#      [ 7, 8, 9,10,11,12],
#      [13,14,15,16,17,18],
#      [19,20,21,22,23,24]]
# or one pass using: https://en.wikipedia.org/wiki/Summed-area_table
# I(x,y) == sum, i(x,y) normal value
# I(x,y) = i(x,y) + I(x,y-1) + I(x-1, y) - I(x-1, y-1)
sums = [[0 for _ in range(len(grid[0]))] for _ in range(len(grid))]
for y in range(len(grid)):
    for x in range(len(grid[0])):
        cur_sum = grid[y][x]
        if y > 0:
            cur_sum += sums[y-1][x]
        if x > 0:
            cur_sum += sums[y][x-1]
        if y > 0 and x > 0:
            cur_sum -= sums[y-1][x-1]
        sums[y][x] = cur_sum
# print("GRIDS EUQAL:", sums == GG)


def areae_sum(tly, tlx, rby, rbx):
    # src: https://www.geeksforgeeks.org/submatrix-sum-queries/
    # rby = right bottom y, rbx = right bottom x, tl = top left...
    # sums from y,x to y+square_size,x+square_size = sums[rby][rbx] - sums[tly-1][rbx]
    # - sums[rby][tlx-1] + sums[tly-1][tlx-1]
    # A O(1) time function to compute sum of submatrix
    # between (tly, tlx) and (rby, rbx) using sums[][]
    # which is built by the preprocess function
    # result is now sum of elements between (0, 0) and
    # (rby, rbx)
    res = sums[rby][rbx]

    # Remove elements between (0, 0) and (tly-1, rbx)
    if tly > 0:
        res = res - sums[tly-1][rbx]

    # Remove elements between (0, 0) and (rby, tlx-1)
    if tlx > 0:
        res = res - sums[rby][tlx-1]

    # Add sums[tly-1][tlx-1] as elements between (0, 0)
    # and (tly-1, tlx-1) are subtracted twice
    if tly > 0 and tlx > 0:
        res = res + sums[tly-1][tlx-1]

    return res


# test all possible grids
max_power = -1e9
max_info = None
for y in range(300):
    print(y, "\r", end="", flush=True)
    for x in range(300):
        # limit square sizes to what still fits in the grid at the current pos
        # e.g. y=299 -> only test 1x1
        # print(x,y,"SQUARE:", min(300 - y, 300 - x))
        for square_size in range(min(300 - y, 300 - x)):
            # print(f"{x},{y},{square_size}", "\r", end="", flush=True)
            if square_size == 0:
                power_total = grid[y][x]
            else:
                power_total = areae_sum(y, x, y + square_size, x + square_size)
            if power_total > max_power:
                max_power = power_total
                # here we use zero-indexing but grid in task uses 1-indexing
                # square_size is just the col/rows we add onto a 1x1 grid -> so square_size + 1
                max_info = (x+1, y+1, square_size+1)

print("Part2:", max_info, max_power)
