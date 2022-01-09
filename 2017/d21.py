with open("d21_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

def flip_v(grid):
    return tuple(reversed(grid))

def flip_h(grid):
    return tuple((s[::-1] for s in grid))

def rotate(grid):
    """Rotates grid by 90° clock-wise"""
    # zip() -> Returns an iterator of tuples, where the i-th tuple contains the i-th element from each of the argument sequences or iterables.
    # --> zip(*grid) -> gives me the columns of grid as tuples of single chars
    # -> col turns into row when rotating but first becomes last -> reverse
    # -> join on "" to get str
    return tuple(("".join(s)[::-1] for s in zip(*grid)))
    # .#.
    # ..#
    # ###
    # longer version
    # rotated = []
    # for n in range(len(grid)):
    #     col_str = ""
    #     for i in range(len(grid)):
    #         # build str of column -> ..#
    #         col_str += grid[i][n]
    #     # reverse col_str since first member of column is the last member of rotated row
    #     # 1st col -> rotated 90° -> 1st row (but order of col reversed)
    #     rotated.append(col_str[::-1])
    # return rotated
                
pattern_dic = {}
for ln in inp.splitlines():
    patt, out = ln.split(" => ")
    key = tuple(patt.split("/"))
    # flipped pattern also matches --> means acutally flipping the grid/square horizontally or vertically 
    # H .#. -> .#.  V .#. -> ###
    #   ..#    #..    ..#    ..#
    #   ###    ###    ###    .#.
    # patterns also match rotated rows:    .#. matches #..
    #                                      ..#         #.#
    #                                      ###         ##.
    # rotating here means acutally rotating the grid/square by 90°
    pattern_dic[key] = out.split("/")


# dont need to store cols as indvidual list items (".","#","." etc)
# since string is also sliceable, hashable etc.
grid = (".#.",
        "..#",
        "###")

def find_match(pattern_dic, square):
    grid = square
    # try rotate and flip combos till key/patter is found in pattern_dic
    for n in range(4):  # 4 rotations
        # supply default if key doesnt exist
        out = pattern_dic.get(grid, None)
        if out:
            break
        # try flipping horizontally and vertically and rotating -> funcs in tuple
        # -> try again with rotating grid -> grid = rotate(grid)
        for func in (flip_v, flip_h):
            out = pattern_dic.get(func(grid), None)
            # pattern match found -> break
            if out:
                break
        if out:
            break
        # set new grid
        grid = rotate(grid)
    
    return out
        
def divide_into_squares(grid, square_size):
    """Divides grid into squares of size square_size x square_size"""
    size = len(grid)
    squares = []
    for row_i in range(0,size,square_size):
        # select square_size nr of rows
        rows = grid[row_i:row_i+square_size]
        for col_i in range(0,size,square_size):
            # select square_size nr of cols starting at col_i(ndex)
            # tuple -> hashable
            square = tuple((row[col_i:col_i+square_size] for row in rows))
            squares.append(square)
    return squares

# now also working
def combine_squares(squares):
    nr_squares = len(squares)
    # only one square -> return as new grid
    if nr_squares == 1:
        return squares[0]
    # squares dont match squares from divide into anymore since they were replaced by output squares from pattern_dic
    square_size = len(squares[0])
    grid = []
    # nr of squares per row/col -> sqrt(nr_squares)
    sq_per_side = int(nr_squares**0.5)
    # nr of rows/cols of resulting grid -> sqrt(nr_squares) * square_size
    size = sq_per_side*square_size
    square_start_ind = 0
    # row index of row in resulting grid
    for row_i in range(size):
        # build row contents
        row = ""
        # when row_i % square_size -> then we extracted all rows of the current "row of squares"
        if row_i % square_size == 0 and (row_i != 0):
            # index of first square of new row -> 4 squares 2x2 -> 0=1st square of 1st row -> 2=1st square of 2nd row of squares or 3rd row of resulting grid
            # rows extracted -> continue with next "row of squares", as in increase index by sq_per_side -> square_start_ind points to first square of current "row of squares" or current row in grid when row_i%square_size==0
            square_start_ind += sq_per_side
        # start at frist square of "row of squares" to end of "row" -> square_start_ind + sq_per_side
        for sq_i in range(square_start_ind,square_start_ind+sq_per_side):
            # row_i % square_size -> row in square that corresponds to current row in result grid
            row += squares[sq_i][row_i%square_size]
        grid.append(row)
    return grid

# solution with pre-generating empty grid
# but minus generating the empty grid both solutions need the same amout of steps
def combine_squares_2(squares):
    nr_squares = len(squares)
    # only one square -> return as new grid
    if nr_squares == 1:
        return squares[0]
    # squares dont match squares from divide into anymore since they were replaced by output squares from pattern_dic
    square_size = len(squares[0])
    # print(squares, square_size)
    # nr of squares per row/col -> sqrt(nr_squares)
    sq_per_side = int(nr_squares**0.5)
    # nr of rows/cols of resulting grid -> sqrt(nr_squares) * square_size
    size = sq_per_side*square_size
    # generate grid with size nr of rows as empty strings
    grid = ["" for n in range(size)]
    # print(grid)

    for i, square in enumerate(squares):
        # row in result grid
        # i-th square // squares per side -> "row" of squares
        # *square_size -> row in result grid that corresponds to first row in square
        row_i_result_grid = i//sq_per_side*square_size
        for n in range(square_size):
            # square_size rows will be written into grid
            # -> row_i_result_grid + n -> row in result grid that corresponds to n-th row in square
            grid[row_i_result_grid + n] += square[n]

    return grid


test = [(".#",
         ".."),

         ("#.",
          "##"),

        ("..", 
         "#."),

        ("##", 
         "..")]

def run(grid, iters):
    size = len(grid)
    for n in range(iters):
        new_grid_squares = []
        if size % 2 == 0:
            squares = divide_into_squares(grid, 2)
            for square in squares:
                # squares are ordered left to right and top to bottom
                new_grid_squares.append(find_match(pattern_dic, square))
        elif size % 3 == 0:
            #new_grid = [[None for n in range(size+3)] for i in range(size+3)]
            squares = divide_into_squares(grid, 3)
            for square in squares:
                new_grid_squares.append(find_match(pattern_dic, square))
        grid = combine_squares_2(new_grid_squares)
        size = len(grid)

    nr_on = sum((sum((1 if c == "#" else 0 for c in ln)) for ln in grid))
    return nr_on

print(run(grid, 5))
print(run(grid, 18))


