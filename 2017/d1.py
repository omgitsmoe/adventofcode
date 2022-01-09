def readfile(fname):
    with open(fname, "r", encoding="UTF-8") as f:
        return f.read()
pinput = readfile("d1_input.txt").strip()

def p1():
    nsum = 0
    prev = None
    if pinput[0] == pinput[-1]:
        nsum += int(pinput[0])
    for n in pinput:
        if n == prev:
            nsum += int(n)
        prev = n

    print(nsum)

def p2():
    p_len = len(pinput)
    print(p_len)
    steps_fwd = p_len//2
    nsum = 0
    
    for i, n in enumerate(pinput):
        # limit index to 0-length of pinput-1 -> by % p_len
        # this limits it to 0..p_len-1 since at multiple of p_len its 0
        # len=6, fwd=3: i=2+fwd=5%6(len)=5, i=3+fwd=6%6=0
        # modulo m limits to 0..m-1
        comp_ind = (i+steps_fwd) % p_len
        print(n, pinput[comp_ind], comp_ind)
        if n == pinput[comp_ind]:
            nsum += int(n)

    print(nsum)

if __name__ == "__main__":
    p2()
