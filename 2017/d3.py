inp = 325489

# n = 8
# step = 0
# nsum = 1
# width = 1
# height = 1
# wh_sum = 1
# while nsum <= inp:
#     nsum += n
#     n += 8
#     step += 1
#     width += 2
#     height += 2
#     wh_sum += (width-2)*2 + height*2
# 
# 
# print(step, nsum, "-> steps to go to input point:", inp-nsum, "\n", n, height, wh_sum)

import math
def p1_mathematical(x):
    # WARNING not working correctly atm, steps for nrs close to corners that are >n steps away from mid are wrong
    # formel für i nach n aufgelöst -> quadratische gleichung: 4n^2 + 4n + (1-i) = 0 (ax^2 + bx + c = 0)
    # -> mitternatchsformel und vereinfachen -> frac_{-4 +- sqrt(16i)}_{8}
    # teilweise wurzelziehen frac_{-4 +- 4*sqrt(i)}_{8} -> -4/8 getrennt schreiben und kürzen
    # -1/2 +- 1/2*sqrt(i) -> 1/2 ausklammern -> 1/2(sqrt(i)-1)
    # nach oben klammern (ceil -> immer aufrunden), da wir x statt i nehmen um n zu berechenen und in einer ebene ist i -> (2n+1)^2 die größte quadratzahl
    # sonst ist nur eine weitere enthalten -> 2 drunter ist direkt wieder die größter der vorherigen ebene (da seitenlänge(a) immer um 2 größer wird
    n = math.ceil(0.5*(math.sqrt(x)-1))
    # biggest nr/numbers in spiral i=1+sum_a=0_to_n(b*8)
    # durch summen rechenregel vereinfachen: 1) egal ob vor oder nach dem summieren mit faktor(fest) multipliziert wird -> *8 vor die summe
    # 2) variable b die von 1,2...n hochlaeuft ist (n(n+1))/2 frac_{n(n+1)}_{2}
    i = 1 + 4 * n *(n + 1)
    a = 2 * n + 1

    # mitte unterste reihe ist: i-n
    # abstand von x zu mitte unten:
    dist_lower_mid = abs(i-n-x)
    # abstand von EINER mitte -> mod seitenlänge-1:
    dist_one_mid = dist_lower_mid % (a-1)
    # max n schritte von der mitte einer seite zur ecke -> limit abstand zu 0..n
    dist_mids = dist_one_mid % (n+1)
    # schritte von mitte einer seite zum zentrum ist immer n
    # max schritte in ecke mit 2n -> abstand von mitte + n -> schritte zum zentrum
    steps = n + dist_mids
    print(f"{steps} from {x} to cent; n {n} i {i} a {a}, distlower {dist_lower_mid} dist_one_mid {dist_one_mid} dist_mids {dist_mids}")
    return steps

# 325489->552, 1024->31, 23->2
print(p1_mathematical(325489))
print(p1_mathematical(1024))
print(p1_mathematical(23))

for n in range(26,50):
    print(n, p1_mathematical(n))


def walk_spiral_to(nr):
    # better as generator
    cur_nr = 1
    x = 0
    y = 0
    step = 1

    while True:
        # right
        for i in range(step):
            x += 1
            cur_nr += 1
            if cur_nr == nr:
                return cur_nr, x, y
        # up
        for i in range(step):
            y += 1
            cur_nr += 1
            if cur_nr == nr:
                return cur_nr, x, y
        # inc step after going up
        step += 1
        # left
        for i in range(step):
            x -= 1
            cur_nr += 1
            if cur_nr == nr:
                return cur_nr, x, y
        # down
        for i in range(step):
            y -= 1
            cur_nr += 1
            if cur_nr == nr:
                return cur_nr, x, y
        # inc step b4 going right again next loop
        step += 1

# cur_nr, x, y = walk_spiral_to(1024)
# print("manhattan dist to", cur_nr, (x,y), "is:", abs(x)+abs(y))

def sum_neighbours(cur, grid):
    # print(cur)
    result = 0
    # upper neighbour
    result += grid[cur[0]-1][cur[1]]
    # below
    result += grid[cur[0]+1][cur[1]]
    # right neighbour
    result += grid[cur[0]][cur[1]+1]
    # left
    result += grid[cur[0]][cur[1]-1]
    # up left
    result += grid[cur[0]-1][cur[1]-1]
    # up right
    result += grid[cur[0]-1][cur[1]+1]
    # down left
    result += grid[cur[0]+1][cur[1]-1]
    # down right
    result += grid[cur[0]+1][cur[1]+1]

    return result

# 147  142  133  122   59
# 304    5    4    2   57
# 330   10    1    1   54
# 351   11   23   25   26
# 362  747  806--->   ...
def p2():
    # uneven nr
    lvls = 11
    # l = [1,1,2,4,5,10,11,23,25,26,54,57,59,122,133,142,147,304,340,351,362,747,806]
    # create square grid
    grid = [[0 for _ in range(lvls)] for _ in range(lvls)]

    # current coords
    # y,x (since 2d list first index is row(y))
    # y-- is going up, y++ going down (since its and index in a list), x normal behaviour
    # lvls // 2 -> center coord
    cur = [lvls // 2, lvls // 2]
    grid[cur[0]][cur[1]] = 1

    cur_nr = 1
    step = 1
    while cur_nr < inp:
        # right
        for i in range(step):
            # advance current coord
            cur[1] += 1
            cur_nr = sum_neighbours(cur, grid)
            # set value in grid
            grid[cur[0]][cur[1]] = cur_nr
            # this could be more elegant -> having just the condition check (for nr being reached) of while
            # -> more advanced way of advancing current coords for spiral gen
            if cur_nr > inp:
                return cur_nr
        # up
        for i in range(step):
            cur[0] -= 1
            cur_nr = sum_neighbours(cur, grid)
            grid[cur[0]][cur[1]] = cur_nr
            if cur_nr > inp:
                return cur_nr
        # inc step after going up
        step += 1
        # left
        for i in range(step):
            cur[1] -= 1
            cur_nr = sum_neighbours(cur, grid)
            grid[cur[0]][cur[1]] = cur_nr
            if cur_nr > inp:
                return cur_nr
        # down
        for i in range(step):
            cur[0] += 1
            cur_nr = sum_neighbours(cur, grid)
            grid[cur[0]][cur[1]] = cur_nr
            if cur_nr > inp:
                return cur_nr
        # inc step b4 going right again next loop
        step += 1



print(p2())


