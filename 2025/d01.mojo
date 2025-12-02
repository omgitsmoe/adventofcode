alias DIAL_STATES = 100
alias START_DIAL_INDEX = 50

# needs a parameterized function to specify the "lifetime"
# of StringSlice
# [] is the parameter list and works kind of like generics or zig comptime
def part1[origin: ImmutOrigin](read lines: List[StringSlice[origin]]) -> None:
    num_zero = 0
    state = START_DIAL_INDEX
    for line in lines:
        offset = Int(line[1:])
        offset *= -1 if line[0] == 'L' else 1
        state = (state + offset) % DIAL_STATES
        if state == 0:
            num_zero += 1

    print("Part1:", num_zero)


fn reaching_zero(current: Int, offset: Int) -> Int:
    if offset == 0:
        return 0

    once: Int
    if offset > 0:
        once = DIAL_STATES - current
    else:
        once = current if current > 0 else DIAL_STATES

    if once > abs(offset):
        return 0

    reaching = 1 + (abs(offset) - once) // DIAL_STATES
    return reaching



def part2[origin: ImmutOrigin](read lines: List[StringSlice[origin]]) -> None:
    num_zero = 0
    state = START_DIAL_INDEX
    for line in lines:
        offset = Int(line[1:])
        offset *= -1 if line[0] == 'L' else 1
        # print("State", state, "Off", offset)
        num_zero += reaching_zero(state, offset)
        # print("  -> reaching", reaching_zero(state, offset))
        state = (state + offset) % DIAL_STATES

    print("Part2:", num_zero)



def main():
    try:
        with open("d01.in", "r") as f:
            contents = f.read()

        # NOTE: splitlines() returns List[StringSlice[mut: Bool //, origin: Origin]]
        #       could not figure out a way to declare part1 such
        #       that it could take that parameter
        #       mut/origin couldn't be inferred and it's not static
        #       and there's nothing available to use origin_of on
        #       even passing as comptime param did not work,
        #       since origin_of(contents) was `contents` but it wanted
        #       `muttoimm contents`....
        #       OK: it needed to be an `ImmutOrigin` specifically
        #       they really have to work on error messages and
        #       Origin/origin_of explanations
        lines = contents.splitlines()

        part1[origin_of(contents)](lines)
        part2[origin_of(contents)](lines)

    except e:
        print("Error:", e)

