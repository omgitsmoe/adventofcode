@fieldwise_init
struct RangeInclusive(Copyable, Movable):
    var min: Int
    var max: Int

    fn contains(self, id: Int) -> Bool:
        if id >= self.min and id <= self.max:
            return True
        return False


fn in_any_range(ranges: List[RangeInclusive], id: Int) -> Bool:
    for r in ranges:
        if r.contains(id):
            return True

    return False


def main():
    try:
        with open("d05.in", "r") as f:
            contents = f.read()

        valid_ranges: List[RangeInclusive] = []
        expect_ranges = True
        num_fresh = 0
        for line in contents.splitlines():
            if not line:
                expect_ranges = False
                continue

            if expect_ranges:
                range_parts = line.split("-")
                valid_ranges.append(
                    RangeInclusive(Int(range_parts[0]), Int(range_parts[1]))
                )
            else:
                ingredient_id = Int(line)
                if in_any_range(valid_ranges, ingredient_id):
                    num_fresh += 1

        print("Part1:", num_fresh)

        # mark as comptime
        # sort by min first then max second
        @parameter
        fn cmp(a: RangeInclusive, b: RangeInclusive) -> Bool:
            return a.min < b.min or (a.min == b.min and a.max < b.max)

        # the compare function is a comptime argument
        # NOTE: sort the list so we can assume r0.min <= r1.min
        sort[cmp](Span(valid_ranges))

        current: RangeInclusive = valid_ranges[0].copy()
        merged: List[RangeInclusive] = []
        for i in range(1, len(valid_ranges)):
            ref next = valid_ranges[i]
            # print("merge with")
            # print("  R", next.min, "-", next.max)

            if current.max >= (next.min - 1):  # allow touching ranges
                current.max = max(current.max, next.max)
            else:
                # gap
                merged.append(current^)
                current = next.copy()
        merged.append(current^)

        all_fresh_ids = 0
        for r in merged:
            print("R", r.min, "-", r.max)
            ids = (r.max + 1) - r.min  # incl end
            all_fresh_ids += ids
            print("  n", ids)

        print("Part2:", all_fresh_ids)
    except e:
        print("Error:", e)
