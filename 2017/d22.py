with open("d22_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

def check_extend_grid(grid, y, x):
    # index error on y at bottom of grid
    if y == len(grid) and y >= 0:
        # append row with uninfected nodes, width matching other rows
        grid.append([0]*len(grid[0]))
    # index err on y at top of grid
    elif y < 0:
        # prepend row with uninfected nodes, width matching other rows
        grid.insert(0, [0]*len(grid[0]))
        # 0 is lowest possible index
        y = 0
    if x == len(grid[0]) and x >= 0:
        # append column (-> append cell to all rows, to keep consistency)
        for row in grid:
            row.append(0)
    elif x < 0:
        # prepend column
        for row in grid:
            row.insert(0, 0)
        x = 0
    # assert(all((len(grid[0]) == len(r) for r in grid)))
    # assert(len(grid[0]) != x)

    # print(y, x, len(grid), len(grid[y]))
    return grid, y, x

def part2():
    inp2 = inp.splitlines()
    # use dict so we can avoid generating and keeping the whole structure in memory
    grid_dic = {}
    for ri, row in enumerate(inp2):
        for ci, c in enumerate(row):
            # use tuple of rowindex, colindex/y,x as key
            grid_dic[(ri,ci)] = c

    burst = 0
    burst_infected = 0
    # y, x
    # possible directions, up right bottom left
    # turning right -> inc index in directions by 1 -> left -1
    directions = ((-1,0), (0,1), (1,0), (0,-1))
    direction = 0
    cur_y = len(inp2)//2
    cur_x = len(inp2[cur_y])//2
    while burst < 10000000:
        burst += 1
        current = (cur_y, cur_x)
        # use get to avoid keyerror, "." as default
        cur_v = grid_dic.get(current, ".")
        # infected node
        if cur_v == "#":
            # turn right
            direction = (direction + 1) % 4
            # current node -> flagged
            grid_dic[current] = "F"
        elif cur_v == "F":
            # reverse direction
            direction = (direction + 2) % 4
            # cleaned
            grid_dic[current] = "."
        elif cur_v == "W":
            # dont turn
            # infected
            grid_dic[current] = "#"
            burst_infected += 1
        elif cur_v == ".":
            # turn left
            direction = (direction - 1) % 4
            # current node -> weakened
            grid_dic[current] = "W" 
        # move forward
        # make sure y and x dont become negative
        cur_y += directions[direction][0]
        cur_x += directions[direction][1]
    print("part2:", burst_infected)

def part1():
    inp1 = [[1 if c == "#" else 0 for c in ln] for ln in inp.splitlines()]
    burst = 0
    burst_infected = 0
    # y, x
    # possible directions, up right bottom left
    # turning right -> inc index in directions by 1 -> left -1
    directions = ((-1,0), (0,1), (1,0), (0,-1))
    direction = 0
    cur_y = len(inp1)//2
    cur_x = len(inp1[cur_y])//2
    while burst < 10000:
        burst += 1
        # infected node
        if inp1[cur_y][cur_x]:
            # turn right
            direction = (direction + 1) % 4
            # current node -> cleaned
            inp1[cur_y][cur_x] = 0
        else:
            # turn left
            direction = (direction - 1) % 4
            # current node -> infected
            inp1[cur_y][cur_x] = 1
            burst_infected += 1
        # move forward
        # make sure y and x dont become negative
        cur_y += directions[direction][0]
        cur_x += directions[direction][1]
        inp1, cur_y, cur_x = check_extend_grid(inp1, cur_y, cur_x)
    print("part1:", burst_infected)
part1()
part2()
