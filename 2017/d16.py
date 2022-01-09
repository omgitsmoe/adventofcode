with open("d16_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read().strip().split(",")
# mb better to parse (convert to int etc) the input first so execution for pt2 is faster

progs = list("abcdefghijklmnop")
# 1billion times too long to bruteforce, since same moves are repeated
# see when order has been seen b4 (original icluded)
seen = [progs[:]]  # can this be a set?
while True:
    for move in inp:
        if move[0] == "s":
            # slice since nr can have 2 digits
            x = int(move[1:])
            # make x progs move to front
            # first slice excludes everything TO the last x members of list and 2nd slice excludes last x members of list
            progs = progs[-x:] + progs[:-x]
        elif move[0] == "x":
            # pos can be 2 digits so rather use split than indexes, then convert to int by mapping int funct to every memember, mb overkill and list comprehension better
            idx_a, idx_b= map(int, move[1:].split("/"))
            # swap places
            # comment from solutions on why this works (used it b4 looking at solutions)
            # Python evaluates the right hand side of an assignment first so you can swap variables with a,b=b,a.
            # You could also unpack your map calls using a,b=map(...)
            progs[idx_b], progs[idx_a] = progs[idx_a], progs[idx_b]
        elif move[0] == "p":
            # get idx of progs to swap
            idx_a = progs.index(move[1])
            idx_b = progs.index(move[3])
            progs[idx_b], progs[idx_a] = progs[idx_a], progs[idx_b]

    # check if seen b4
    if progs in seen:
        # nr of steps it takes to start repeating reached, but do we have to make sure its acutally repeating all over again and didnt just reach one state that has been seen b4 and then keeps on generating new ones?
        # NO just the first repetition is enough since we now the dance moves its gonna do and that first one after repetition will match the answer of part1 (after finishing dance movesin inp once; first rep then is original)
        # -> if progs == seen[0] would also work
        break
    # be careful to append a copy, since lists are mutable and inner lists will change state if we modify progs
    seen.append(progs[:])

print("".join(seen[1]))
# lenght of seen is nr of steps it takes for order to start repeating
# -> 1billion % length is the step we are in between one rep and the next == index of order in list that equals order after 1billion dances
print("".join(seen[1000000000 % len(seen)]))
