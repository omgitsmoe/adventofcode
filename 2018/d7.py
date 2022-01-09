from collections import defaultdict
from utils import readfile

inp = readfile("d7_input.txt").strip().splitlines()

steps = defaultdict(list)
for ln in inp:
    before, step = ln[5], ln[36]
    steps[step].append(before)
    # also add before step to list since it might not have any
    # b4 steps
    if before not in steps:
        steps[before] = []

# find starting step!! SINGULAR!!
# min() works for alphabetical sorting IF all letters are either
# upper of lower case (since ord(letter) is compared)
# multiple steps that are ready -> use alphabetically first
# DONT add multiple steps at once only add first alhapbetically
# since other steps might become ready
order = [ min((st for st in steps if not steps[st])) ]
while True:
    ready = []
    # loop through steps and collect those that are ready
    for step, before_list in steps.items():
        # so we dont have to delete them from steps
        if step in order:
            continue
        # rdy if all steps that need to be finished before are in order
        rdy = all((True if b4 in order else False for b4 in before_list))
        if rdy:
            ready.append(step)
    if not ready:
        break
    # take next step that comes first alhpabetically
    order.append(min(ready))
print("PART 1:", "".join(order))

# PART 2
order = []
workers = [(0, None) for _ in range(5)]
# with current starting at 0: 981 found; but 980 correct -> OFF BY ONE ERROR
# -1 so we start at second 0 with assigning the steps to workers
# example also starts at second 0
current = -1
steps_found = True
while steps_found:
    # advance time
    current += 1
    for i, (worker_ts, worker_st) in enumerate(workers):
        # worker not rdy yet
        if worker_ts > current:
            continue
        # step finished
        if worker_st:
            order.append(worker_st)
        ready = []
        for step, before_list in steps.items():
            # so we dont have to delete them from steps
            if step in order or any(step in w for w in workers):
                continue
            rdy = (all((True if b4 in order else False for b4 in before_list))
                   if before_list else True)
            if rdy:
                ready.append(step)

        if not ready:
            # workers still working; >= important since worker_ts == curent means hes not done yet
            if any(w[0] >= current for w in workers):
                # no new step found -> worker has to wait -> set his step to None
                workers[i] = (worker_ts, None)
                continue
            # workers not working and no steps found -> done
            steps_found = False
            break

        next_step = min(ready)
        # steps take 60 secs + pos of letter in alphabet (1-indexed); ord("A") == 65
        seconds_needed = 60 + ord(next_step) - 65 + 1
        workers[i] = (current + seconds_needed, next_step)
# if we didnt find any steps workers may not have finished theirs
# so get max timestamp from workers which == end time
print("PART 2:", max(workers, key=lambda x: x[0])[0])
