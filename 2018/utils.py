def readfile(fname):
    with open(fname, "r", encoding="UTF-8") as f:
        return f.read()