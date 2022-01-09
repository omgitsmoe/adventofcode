f = open("d02.in", "r")
codes_o = [int(x) for x in f.read().split(",")]
f.close()

codes = [x for x in codes_o]
codes[1] = 12
codes[2] = 2

def add(x, y, out):
    codes[out] = codes[x] + codes[y]
def mult(x, y, out):
    codes[out] = codes[x] * codes[y]

i = 0
while True:
    opcode = codes[i]
    if opcode == 99:
        break
    elif opcode == 1:
        add(codes[i+1], codes[i+2], codes[i+3])
    elif opcode == 2:
        mult(codes[i+1], codes[i+2], codes[i+3])
    else:
        print("ERROR")
        break
    i += 4

print("Part1:", codes[0])

# PART 2
for noun in range(100):
    for verb in range(100):
        codes = [x for x in codes_o]
        codes[1] = noun
        codes[2] = verb
        i = 0
        while True:
            opcode = codes[i]
            if opcode == 99:
                break
            elif opcode == 1:
                add(codes[i+1], codes[i+2], codes[i+3])
            elif opcode == 2:
                mult(codes[i+1], codes[i+2], codes[i+3])
            else:
                print("ERROR")
                break
            i += 4

        if codes[0] == 19690720:
            print("Part2:", 100 * noun + verb)
            break
        # print("N V:", noun, verb, "Out", codes[0])
