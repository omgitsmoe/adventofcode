def find_largest_digit(window: StringSlice) -> Tuple[Int, Int]:
    max_window = 0
    max_window_idx = -1
    # StringSlice not Iterable :/
    for i in range(len(window)):
        num = Int(window[i])
        if num > max_window:
            max_window = num
            max_window_idx = i

    return max_window, max_window_idx


# We try to find the n largest digits in order.
#
# Earlier digets are more significant by an order of magniture,
# so we search the each sub-window for the largest digit.
# The window is constrained such that at least the remaining
# window_size can be fullfilled.
# After finding the largest we just narrow the new search window
# to all digits after the largest we found.
def find_n_largest(var window: StringSlice, window_size: Int) -> Int:
    largest = ""
    for _ in range(window_size):
        # remaining after we find the current one
        remaining = window_size - len(largest) - 1
        search_window = window[:-remaining] if remaining > 0 else window

        max_in_window, max_idx = find_largest_digit(search_window)
        largest += String(max_in_window)
        window = window[max_idx + 1:]

    return Int(largest)

# use a more efficient greedy stack-based algorithm for
# the "greedy subsequence" problem
def find_n_largest_stack(s: StringSlice, n: Int) -> Int:
    stack: List[Int] = []
    remaining = len(s)  # digits left to process

    for i in range(len(s)):
        digit = s[i]
        d = Int(digit)
        # While stack is non-empty, last digit < current,
        # and we have enough remaining digits to still fill n
        # -> remove all digits that are smaller than the current one
        #    till we are limited by the remaining digits to fill
        while stack and stack[-1] < d and len(stack) + remaining - 1 >= n:
            _ = stack.pop()
        if len(stack) < n:
            stack.append(d)
        remaining -= 1

    # Convert first n digits to integer
    result = ""
    for d in stack[:n]:
        result += String(d)
    return Int(result)


alias BATTERIES_PART2 = 12
def main():
    try:
        with open("d03.in", "r") as f:
            contents = f.read()

        total_joltage = 0
        total_joltage_part2 = 0
        for bank in contents.splitlines():
            print(bank)
            # old part1
            # first_digit, first_digit_idx = find_largest_digit(bank[:-1])
            # second_digit, _ = find_largest_digit(bank[first_digit_idx + 1:])
            # joltage = Int(String(first_digit, second_digit))

            joltage = find_n_largest(bank, 2)
            total_joltage += joltage

            joltage = find_n_largest_stack(bank, BATTERIES_PART2)
            total_joltage_part2 += joltage
            print("Max", joltage)

        print("Part1:", total_joltage)
        print("Part2:", total_joltage_part2)
    except e:
        print("Error:", e)

