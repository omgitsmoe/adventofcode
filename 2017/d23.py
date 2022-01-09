with open("d23_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

inp = [ln.split() for ln in inp.splitlines()]

def return_or_convert(x, dic):
    # x/y in instructs can be number or register -> try using as number -> ValueError if register
    try:
        return int(x)
    except ValueError:
        return dic[x]

regs = {reg: 0 for reg in list("abcdefgh")}
n = 0
mul_counter = 0
while 0 <= n < len(inp):
    cmd, x, y = inp[n]

    if cmd == "set":
        regs[x] = return_or_convert(y, regs)
    elif cmd == "sub":
        regs[x] -= return_or_convert(y, regs)
    elif cmd == "mul":
        mul_counter += 1
        regs[x] *= return_or_convert(y, regs)
    elif cmd == "jnz":
        if return_or_convert(x, regs) != 0:
            n += return_or_convert(y, regs)
            # continue with next iteration of loop since we already jumped and dont need to increase n
            continue

    n += 1
print("part1:",mul_counter)

# part2
# ln 26: h+1 if f==0 -> f=0 if b is not a prime number due to lines 13-15 (g*e==b since g==0 needed to set f to 0)
# -> check if any of e(counting up from 2 to b(<-excluded)) evenly divides b -> yes -> h+1
# -> due to line 30 and 33 this gets repeated while b is increasing by 17 till b==c
# -> so 1000(+1 since starting nr is also tested) times since c-b=17000
h = 0
for b in range(109900, 126900 + 1, 17):
    for e in range(2, b):
        if b % e == 0:
            h += 1
            break
print("part2:",h)

