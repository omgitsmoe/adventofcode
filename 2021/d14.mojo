import time

# NOTE: converted from Python using ChatGPT
#       then fixed all errors/outdated syntax
# all versions exactly follow the algorithm used by d14.py to be comparable
# with I/O:
#   python: ~14ms
#   cpp: ~1.7ms (g++ -std=c++17 -O3 -march=native -o d14_cpp d14.cpp)
#   mojo: ~16ms (mojo build then `time ./d14`)
# without I/O:
#   python: ~0.75ms
#   cpp: ~0.143ms (g++ -std=c++17 -O3 -march=native -o d14_cpp d14.cpp)
#   mojo: ~0.43ms (mojo build)

alias StrIntDict = Dict[String, Int]
alias RulesMap = Dict[String, Tuple[String, String]]

# ------------------------------------------------------------
# Read file using Mojo's native I/O
# ------------------------------------------------------------
def read_file_to_lines(path: String) -> List[String]:
    with open(path, "r") as f:
        data = f.read()
    return [String(s) for s in data.splitlines()]

# ------------------------------------------------------------
# Apply rules using pair counting
# ------------------------------------------------------------
def apply_rules(polymer: String, rules: RulesMap, times: Int) -> StrIntDict:
    var pair_count: StrIntDict = {}

    # initial pair counts
    for i in range(len(polymer) - 1):
        p = polymer[i:i+2]
        pair_count[p] = pair_count.get(p, 0) + 1

    # simulation steps
    for _ in range(times):
        var new_pair_count = pair_count.copy()

        for entry in rules.items():
            ref src = entry.key
            ref pairs = entry.value

            if not src in pair_count:
                continue

            count = pair_count[src]
            (p1, p2) = pairs

            new_pair_count[src] = new_pair_count.get(src, 0) - count
            new_pair_count[p1]  = new_pair_count.get(p1, 0) + count
            new_pair_count[p2]  = new_pair_count.get(p2, 0) + count

        pair_count = new_pair_count^

    return pair_count^

# ------------------------------------------------------------
# Count characters from pair frequencies
# ------------------------------------------------------------
def count_chars(pairs: StrIntDict) -> StrIntDict:
    var char_count: StrIntDict = {}

    for entry in pairs.items():
        ref pair = entry.key
        ref count = entry.value
        for c in pair.as_bytes():                    # c is a Char
            s = String(c)
            char_count[s] = char_count.get(s, 0) + count

    return char_count^

# ------------------------------------------------------------
# Each character except endpoints counted twice
# ------------------------------------------------------------
def correct_counts(char_count: StrIntDict) -> StrIntDict:
    var result: StrIntDict = {}

    for entry in char_count.items():
        ref k = entry.key
        ref v = entry.value
        result[k] = Int(v / 2.0 + 0.5)
    return result^

# ------------------------------------------------------------
# Entry point
# ------------------------------------------------------------
fn main():

    try:
        lines = read_file_to_lines("d14.in")

        polymer = lines[0]
        rules_lines = lines[2:]

        # Build rule map
        var rulesMap: RulesMap = {}
        for r in rules_lines:
            line = r.strip()
            if line == "":
                continue

            parts = line.split(" -> ")
            src = parts[0]
            ins = parts[1]

            p1 = src[0:1] + ins
            p2 = ins + src[1:2]
            rulesMap[String(src)] = (p1, p2)

        # Process polymer
        start = time.perf_counter()
        pairs = apply_rules(polymer, rulesMap, 40)
        counts = correct_counts(count_chars(pairs))

        # Compute result
        var min_val = Int.MAX
        var max_val = Int.MIN
        for entry in counts.items():
            ref v = entry.value
            if v < min_val: min_val = v
            if v > max_val: max_val = v

        print("Part2: ", String(max_val - min_val))

        end = time.perf_counter() # in seconds
        ms = (end - start) * 1000
        print("Took ", String(ms), " ms")
    except e:
        print("Exception", e)
