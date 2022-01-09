with open("d02_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

# keypad = {(0,0): 1, (1,0): 2, (2,0): 3,
#           (0,1): 4, (1,1): 5, (2,1): 6,
#           (0,2): 7, (1,2): 8, (2,2): 9}
keypad = [[1,2,3],
          [4,5,6],
          [7,8,9]]
x=1
y=1
code = []

for ln in inp.splitlines():
    for cmd in ln:
        if cmd == "U":
            y-=1
        elif cmd == "D":
            y += 1
        elif cmd == "L":
            x -= 1
        elif cmd == "R":
            x += 1

        # i was using % here before but i dont wann wrap i wanna stay at that number
        # till a instruction takes me to a valid index
        x = min(2, x)
        x = max(0, x)
        y = min(2, y)
        y = max(0, y)
    code.append(keypad[y][x])
print("".join(map(str, code)))

keypad2 = [[0,0,1,0,0],  
           [0,2,3,4,0],
           [5,6,7,8,9],
           [0,"A","B","C",0],
           [0,0,"D",0,0]]

x=0
y=2
maxx=4
maxy=4
code = []

for ln in inp.splitlines():
    for cmd in ln:
        if cmd == "U":
            if (y > 0) and (keypad2[y-1][x] != 0):
                y-=1
        elif cmd == "D":
            if (y < maxy) and (keypad2[y+1][x] != 0):
                y += 1
        elif cmd == "L":
            if (x > 0) and (keypad2[y][x-1] != 0):
                x -= 1
        elif cmd == "R":
            if (x < maxy) and (keypad2[y][x+1] != 0):
                x += 1

    code.append(keypad2[y][x])
print("".join(map(str, code)))

