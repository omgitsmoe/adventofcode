from collections import defaultdict

from utils import readfile

inp = readfile("d4_input.txt").strip().splitlines()
# also works with just using the whole string
inp = sorted(inp, key=lambda x: x[:20])

# wouldve been easier if we used sep dicts for minutes slept total and guard,minute
guards = defaultdict(list)

current_guard = None
asleep = False
for ln in inp:
    date_time, action = ln.split("] ")
    _, time = date_time.split(" ")
    hours, minutes = time.split(":")
    hours, minutes = int(hours), int(minutes)
    if action.startswith("Guard"):
        if asleep:
            raise Exception("New guard when last one is still asleep!")
        current_guard = int(action.split("#")[1].split(" ")[0])
    elif action.startswith("falls"):
        asleep = minutes
    elif action.startswith("wakes"):
        if asleep is False:
            raise Exception("Guard waking but he wasnt asleep!")
        # wake time already counts as awake
        guards[current_guard].extend(range(asleep, minutes))
        asleep = False
# get guard_id with most minutes asleep (length of minutes asleep list)
most_asleep = max(guards, key=lambda x: len(guards[x]))
# get the minute this guard is aslepp the most, using the lists count function
# as key
minute_asleep_most = max(guards[most_asleep], key=guards[most_asleep].count)
print("Part1:", most_asleep*minute_asleep_most)

# useless unless we have some concise way to get the minute that a guard is asleep the most
# from it without using a for loop since then we can just generate this data in a for loop
# minute_spent_asleep = {guard_id: {minute: asleep_list.count(minute) for minute in asleep_list}
#                        for guard_id, asleep_list in guards.items()}
max_current = 0
max_minute = None
current_guard = None
for guard_id, minutes_asleep in guards.items():
    for minute in minutes_asleep:
        count = minutes_asleep.count(minute)
        if count > max_current:
            max_current = count
            max_minute = minute
            current_guard = guard_id
print("Part2:", current_guard*max_minute)
