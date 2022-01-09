from collections import deque
# i thought pythons built-in list used a linked-list implementation
# but apparently (as by the docs of blist package) it uses a
# dynamically sized array -> that's why inserting elements into
# a big list is so slow here => use blist/own implementation of linked list or
# deque
# also in deque documentation:
# Though list objects support similar operations, they are optimized for fast
# fixed-length operations and incur O(n) memory movement costs for pop(0) and
# insert(0, v) operations which change both the size and position of the
# underlying data representation.

inp = "493 players; last marble is worth 71863 points"
players_nr = 493
last_marble_pts = 71863

circ = [0]
players = [0 for _ in range(players_nr)]

player = 0
marble = 0
marble_idx = 0
while marble < last_marble_pts:
    player = (player + 1) % players_nr
    marble += 1
    if marble % 23 == 0:
        # add marble to score
        players[player] += marble
        # marble that is 7 marbles counter-clockwise from current
        # removed + added to score
        marble_7_ccw_idx = (marble_idx - 7) % len(circ)
        players[player] += circ.pop(marble_7_ccw_idx)
        # marble clockwise from removed -> new current
        marble_idx = marble_7_ccw_idx
    else:
        # place marble between pos 1 and 2 marbles clockwise from current
        left_i = (marble_idx + 1) % len(circ)
        right_i = (marble_idx + 2) % len(circ)
        # reached boundary of current circ or len(circ)==1
        if right_i <= left_i:
            circ.append(marble)
            marble_idx = left_i + 1
        else:
            #       l  r
            # 0 (4) 2  1  3
            # insert(3, ..)
            # => 0 4 2 (5) 1  3
            circ.insert(right_i, marble)
            marble_idx = right_i

print("Part1:", max(players))

# part2
last_marble_pts *= 100

circ = deque([0])
players = [0 for _ in range(players_nr)]

player = 0
marble = 0
while marble < last_marble_pts:
    player = (player + 1) % players_nr
    marble += 1
    if marble % 23 == 0:
        # add marble to score
        # marble that is 7 marbles counter-clockwise from current
        # removed + added to score
        # rotate deque by 7 to the right -> 7 marbles counter-clock-wise (ccw)
        # is now at right end of deque
        circ.rotate(7)
        players[player] += marble + circ.pop()
        # marble clockwise from removed -> new current
        circ.rotate(-1)
    else:
        # place marble between pos 1 and 2 marbles clockwise from current
        # rotate deque by 1 to the left -> 1 marble clockwise from current is now
        # right end of deque
        circ.rotate(-1)
        circ.append(marble)

print("Part2:", max(players))
