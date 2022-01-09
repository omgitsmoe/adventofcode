with open("d01_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

inp = [(cmd[0], int(cmd[1:])) for cmd in inp.split(", ")]

# only decreased dist when we were travelling south but if we eg travelled east and then back west later
# wed also have to decrease dist
# -> better to use coords and calc manhattan dist at the end
dirs = ((0,1), (1,0), (0,-1), (-1,0))
x =0
y=0
dir_i = 0

for di,steps in inp:
    inc = 1 if di == "R" else -1
    dir_i = (dir_i + inc) % len(dirs)
    dx,dy = dirs[dir_i]
    x+=dx*steps
    y+=dy*steps

print(abs(x)+abs(y))

x =0
y=0
dir_i = 0
seen = set((0,0))
found = False
for di,steps in inp:
    inc = 1 if di == "R" else -1
    dir_i = (dir_i + inc) % len(dirs)
    dx,dy = dirs[dir_i]
    # dont walk the steps at once!! walk them one by one and add coords to seen etc
    for _ in range(steps):
        x+=dx
        y+=dy
        if (x,y) in seen:
            # set found so we can break of outer loop as well
            found=True
            break
        else:
            seen.add((x,y))
    if found:
        break

print(abs(x)+abs(y))
