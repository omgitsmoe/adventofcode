import time
from typing import Dict, Tuple

with open("d14.in", "r") as f:
    contents = f.readlines()
polymer = contents[0].strip()
rules = contents[2:]

rulesMap: Dict[str, Tuple[str, str]] = {}
for rule in rules:
    src_pair, insert = rule.strip().split(" -> ")
    rulesMap[src_pair] = (f"{src_pair[0]}{insert}", f"{insert}{src_pair[1]}")

def apply_rules(polymer: str, rules: Dict[str, Tuple[str, str]], times: int) -> Dict[str, int]:
    pair_count: Dict[str, int] = {}
    for a, b in zip(polymer, polymer[1:]):
        pair = f"{a}{b}"
        pair_count[pair] = pair_count.get(pair, 0) + 1
    
    for _ in range(times):
        new_pair_count = pair_count.copy()
        for (src_pair, (pair1, pair2)) in rules.items():
            if src_pair not in pair_count:
                continue
            
            count = pair_count[src_pair]
            new_pair_count[src_pair] -= count
            new_pair_count[pair1] = new_pair_count.get(pair1, 0) + count
            new_pair_count[pair2] = new_pair_count.get(pair2, 0) + count
        
        pair_count = new_pair_count
    
    return pair_count

def count_chars(pairs: Dict[str, int]) -> Dict[str, int]:
    char_count: Dict[str, int] = {}
    for pair, count in pairs.items():
        for char in pair:
            char_count[char] = char_count.get(char, 0) + count
    return char_count

def correct_counts(char_count: Dict[str, int]) -> Dict[str, int]:
    return dict(map(lambda kv: (kv[0], int(kv[1] / 2 + 0.5)), char_count.items()))

# python and julia are fastest between python, julia and F#
# python: 2ms julia: 2ms (applyrules functional reduce 50ms) F#: 27ms
# very surprising that python is just as fast as julia
start = time.time()
pairs = apply_rules(polymer, rulesMap, 40)
counts = correct_counts(count_chars(pairs))
print("Part2:", max(counts.values()) - min(counts.values()))
end = time.time()
took = end-start
tookms = took*1000
print("Took", took, "seconds ->", tookms, "ms")
