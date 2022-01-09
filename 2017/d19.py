with open("d19_input.txt", "r", encoding="UTF-8") as f:
    # whitespace important
    inp = f.read().rstrip()

# inp = \
# """     |          
#      |  +--+    
#      A  |  C    
#  F---|----E|--+ 
#      |  |  |  D 
#      +B-+  +--+""" 

def get_gridpos(y, x):
    try:
        return inp[y][x]
    except IndexError:
        return " "

inp = inp.splitlines()
# down, up, right, left
# moves = [(1,0), (-1,0), (0,1), (0,-1)]
direction = (1,0)
# get index x of starting point
pos = (0, inp[0].index("|"))  # y,x
letters = []
steps = 0

while True:
    steps += 1
    y, x = pos
    dy, dx = direction

    # instead of limiting y or x to the range of inp (% len(inp)...) using function that tries to return it and excepts IndexError and returns " " on that
    val_cur = get_gridpos(y, x)
    # print(val_cur)
    if val_cur.isalpha():
        # alphanumerisch -> buchstabe (oder nummer..)
        letters.append(val_cur)

    # have to exclude going back the way we came and going towards directions that are impossible von | rechts nach | z.b. -> check dx, dy
    # prob would have missed the latter one (if i didnt glimpse at one solution), but quickly able to find the error by printing value (which i did anyway)
    # bool(" ") > True -> check for != " "
    if get_gridpos(y + dy, x + dx) != " ":
        pos = (y + dy, x + dx)
    # wenn dx != 0 -> was moving horizontally b4 turning left or right > move vertically and so on
    elif dx and get_gridpos(y + 1, x) != " ":
        pos = (y + 1, x)
        direction = (1, 0)
    elif dx and get_gridpos(y - 1, x) != " ":
        pos = (y - 1, x)
        direction = (-1, 0)
    elif dy and get_gridpos(y, x + 1) != " ":
        pos = (y, x + 1)
        direction = (0, 1)
    elif dy and get_gridpos(y, x - 1) != " ":
        pos = (y, x - 1)
        direction = (0, -1)
    else:
        # end reached
        break

print("part1:", "".join(letters))
print("part2:", steps)
