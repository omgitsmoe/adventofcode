@fieldwise_init
struct Range(Copyable, Movable,  # without this we can't use it in a List
             Writable):
    var first_id: Int
    var last_id: Int

    fn write_to(self, mut writer: Some[Writer]):
        var string = "Range"
        writer.write_bytes(string.as_bytes())
        # Pass multiple args that can be converted to a `Range`:
        writer.write("(", self.first_id, ", ", self.last_id, ")")


fn is_invalid(num: Int) -> Bool:
    as_str = String(num)
    if (len(as_str) % 2) != 0:
        return False

    middle = len(as_str) // 2
    first = as_str[:middle]
    second = as_str[middle:]
    if first == second:
        return True

    return False


fn is_invalid_part2(num: Int) -> Bool:
    as_str = String(num)

    for window_size in range(1, len(as_str) // 2 + 1):
        window_count, remainder = divmod(len(as_str), window_size)
        if remainder != 0:
            continue

        first_window = as_str[:window_size]
        for i in range(window_count - 1):
            window_index = i + 1
            current_window = as_str[window_size*window_index:window_size*(window_index + 1)]
            if first_window != current_window:
                break
        else:
            # for executed without break
            return True

    return False


def main():
    try:
        with open("d02.in", "r") as f:
            contents = f.read()

        ranges: List[Range] = []
        for range_str in contents.split(","):
            split = range_str.split("-")
            ranges.append(Range(
                Int(split[0]),
                Int(split[1]),
            ))

        num_invalid = 0
        num_invalid_part2 = 0
        for r in ranges:
            for n in range(r.first_id, r.last_id + 1):
                if is_invalid(n):
                    # print("IsInvalid", n)
                    num_invalid += n
                if is_invalid_part2(n):
                    # print("IsInvalid", n)
                    num_invalid_part2 += n

        print("Part1:", num_invalid)
        print("Part2:", num_invalid_part2)
    except e:
        print("Error:", e)

