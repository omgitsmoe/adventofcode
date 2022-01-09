inp = open("d08.in").read().strip()
w, h = 25, 6

layers = []
ptr = 0
while ptr < len(inp):
    layer = []
    for y in range(h):
        row = []
        for x in range(w):
            row.append(inp[ptr])
            ptr += 1
        layer.append(row)
    layers.append(layer)

cd = lambda l, d: sum(sum(col == d for col in row) for row in l)
fewest0 = []
min_zeroes = 10e9
for layer in layers:
    n_zeroes = cd(layer, '0')
    if n_zeroes < min_zeroes:
        min_zeroes = n_zeroes
        fewest0 = layer
print("PART1:", cd(fewest0, '1') * cd(fewest0, '2'))

img = [['2' for x in range(w)] for y in range(h)]
for i in range(len(layers)-1, -1, -1):
    assert len(layers[i]) == h
    for y in range(h):
        assert len(layers[i][y]) == w
        for x in range(w):
            # skip transparent
            if layers[i][y][x] != '2':
                # white as X, black as blank
                img[y][x] = 'X' if layers[i][y][x] == '1' else ' '
print("PART2:")
print("\n".join("".join(d) for d in img))
    


