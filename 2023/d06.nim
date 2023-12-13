import std/sugar, std/strutils, std/sequtils, std/algorithm, std/math

let
    input_file = "d06_input.txt"
    # input_file = "d06_example.txt"
    input = (readFile input_file).split('\n')
    times = input[0].split(' ')
        .filter(w => len(w) > 0 and isDigit w[0])
        .map(parseInt)
    distances = input[1].split(' ')
        .filter(w => len(w) > 0 and isDigit w[0])
        .map(parseInt)

# proc distance_a(starting_velocity: float, time_ms: float, acceleration: float): float =
#     # Distance = v*t + 1/2*a*t^2
#     # Where v is the velocity, t is time, and a is the acceleration.
#     return starting_velocity * time_ms + 1/2 * acceleration * time_ms**2

proc distance(velocity: float, time_ms: float): float =
    # Distance = v*t
    return velocity * time_ms;

proc winning_variations(time: int, record_dist: int): int =
    # walk in one direction till d <= record
    # distance walked*2 (since each step from the max will reach the same
    # distance on each side, e.g. time 10ms max 25mm at 5ms hold time
    # -> 4ms=24mm and 6mm=24mm
    let
        # max dist reachable with: hold_ms = time / 2
        # + ceil/floor if odd
        # cast[float]() is bitwise, use float() instead
        max_dist_hold_ms = int(floor(float(time) / 2.0))
    var
        # two max hold times if odd, e.g. [4, 6] from example above
        winning_variations = if time mod 2 != 0: 2 else: 1
        # steps/ms from the hold time that reaches the maximum
        dt_ms_from_max = 1
    while true:
        let
            hold_time = max_dist_hold_ms - dt_ms_from_max
            dist = (time - hold_time) * hold_time
        # echo "hold ", hold_time, " dist ", dist
        if dist <= record_dist:
            break

        dt_ms_from_max += 1
        # 1 per side
        winning_variations += 2
    # echo time, "ms rec: ", record_dist, "mm maxhold ", max_dist_hold_ms, " -> variations: ", winning_variations

    return winning_variations

var part1 = 1
for (time, record_dist) in zip(times, distances):
    part1 *= winning_variations(time, record_dist)

echo "Part1: ", part1

let
    time_pt2 = input[0].replace(" ", "").split(':')[1].parseInt
    distance_pt2 = input[1].replace(" ", "").split(':')[1].parseInt
echo "Part2: ", winning_variations(time_pt2, distance_pt2)

