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
    s = String(num)

    # NOTE: A string is made of repeated substrings if it appears
    #       inside a rotated version of itself.
    #       So a string s is composed of repeated substrings if and only if:
    #       `s is found in (s + s)[1 : -1]`
    #       `s + s` contains all rotated versions of itself:
    #       s = "abcd"
    #       s + s = "abcdabcd"
    #       abcd   # original
    #       bcda   # rotated left 1
    #       cdab   # rotated left 2
    #       dabc   # rotated left 3
    #       abcd   # original again
    #
    #       Repitiions matter, since they keep the boundaries aligned
    #       original: abcabc
    #       rotate 3: abcabc   # looks identical!
    #
    #       If we take s + s and remove the first and last character, we
    #       eliminate the trivial occurrences of s.
    #       But if s is made of smaller repeating blocks, the internal
    #       rotations recreate s somewhere inside.
    #       s + s = "abcabcabcabc"
    #       (s + s)[1:-1] = "bcabcabcabc"
    #       s in ^ == True
    #
    #       s + s = "abcdabcd"
    #       (s + s)[1:-1] = "bcdabcd"
    #       s in ^ == False
    return s in (s + s)[1:-1]


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

