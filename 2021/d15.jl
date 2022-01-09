# open exectutes the function passed as first param and then closes the file
contents = split(strip(open(f -> read(f, String), "d15.in")), "\n")

const Pos = Tuple{Int, Int}
# for types to be mutable they need to be declared as such:
mutable struct Node
    level :: Int
    risk_sum :: Int64
    prev :: Pos
end

# linear flat arr: grid = [Node(Int(c), typemax(Int64), (-1, -1)) for l = contents for c = l]
# column-major arrays is fucky since text/graphics is row major,
# but linear algebra is column-major (Fortran, Matlabl, BLAS etc.)
# but indexing is still row,column
# this will generate 1d array (in the julia sense) with 1d arrays as elements
# Vector is alias for 1d array: Vector{Vector{Node}}
# which we can access like in C: grid[row][column], but we can't use grid[row,column]
# that only works for "multi-dimensional arrays"
# NOTE: would this be bad if I were to iterate over it by row first and then col?
# I don't think so since the arrays should be contiguous in the order I specified
# here which is row-major, only using the built-in multi-dimensional array it would
# be bad (there it would be col then row)
# FUCKING CHAR TYPE IN JULIA Int('1') -> 49... OMGOMOGOGMO
# ...and Int(::String) was removed due to the confusion...WOW but not the one
# that actually causes the confusion
# this language ACTUALLY succeeds in doing the most random things differently than
# anybody else but not doing it better just weirder
# (char in c is ~fine since it's statically typed)
grid = [[Node(parse(Int, c), typemax(Int64), (-1, -1)) for c = l] for l = contents]
dimy = size(grid, 1)
dimx = size(grid[1], 1)

function binsearch(arr, x)
    last = size(arr, 1)
    first = 1

    while first < last
        mid = trunc(Int, first + (last - first) / 2)
        # hard coded 2nd element which is the cost here
        val = arr[mid][2]
        # inserting 2
        # 54331
        # f ^ l
        # 54331
        #   f^l
        # 54331
        #    fl -> first == mid
        if val == x
            return mid
        # mid value is higher, use mid as new start of the search window
        elseif val > x
            # mid will fall onto first when last/first are right next to eachother
            # so if they are the same and the value is still higher we can
            # return last (since we want a descending sort, so smallest number is at
            # the end of the arr -> we have to be __at least__ one to the right of
            # first/mid which is last)
            if first == mid
                # check if last is smaller -> insert at/before (insert moves the
                # element at inseted idx one to the right)
                # last is greater -> one to the right of last
                return arr[last][2] < x ? last : last + 1
            end
            first = mid
        else
            # val at mid is lower move end of the search window to current mid
            last = mid
        end
    end

    # reached start of arr (or [] or [y]) and no value was greater
    1
end

# array literals: [[1, 2], [2, 3]] -> 2-element Vector{Vector{Int64}}
# since comma separates elements
# ; or \n -> concat vertically
# [1:2; 4:5] -> 4-element Vector{Int64} [1, 2, 4, 5]
# or
# 1
# 2
# 4
# 5
# tabs or spaces or double semicolons -> horizontal concat
# [[1,2]  [4,5]  [7,8]] -> 2×3 Matrix{Int64} [[1, 4, 7], [2, 5, 8]]
# or:
# 1  4  7
# 2  5  8
# julia uses column-major order (columns are contiguous in memory;
# C and Python use row-major)
# cartesan indexing: e.g. 2d from above
# A[2, 1] -> 2; 2nd row 1st column (internally/in memory this would be A[1][2])
# linear indexing:
# A[5] -> 7; counts linearly in column order
# Array{T, N} N-dimensional array with elements of type T
# ! at end of function name -> mutate at least one of the args
function findPath!(grid::Vector{Vector{Node}}, start::Pos, end_pos::Pos)
    # 2nd param is dimension
    dimy = size(grid, 1)
    dimx = size(grid[1], 1)
    # sorted: last item should be the one with the lowest riskSum
    q::Vector{Tuple{Pos, Int64}} = [(start, 0)]
    visited::Set{Pos} = Set([start])

    while size(q, 1) > 0
        pos, path_cost = pop!(q)
        (x, y) = pos

        if pos == end_pos
            return path_cost
        end

        # mark visited
        push!(visited, pos)
        neighbours = filter(((x, y)::Tuple{Int, Int}) -> x > 0 && x <= dimx && y > 0 && y <= dimy,
                            [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)])
        for npos in neighbours
            if npos in visited
                # should already have optimal cost
                continue
            end

            ny, nx = npos
            neighbour_node = grid[ny][nx]
            #println(neighbour_node)
            new_cost = path_cost + neighbour_node.level
            if new_cost < neighbour_node.risk_sum
                # cheaper path -> update in grid and queue
                neighbour_node.risk_sum = new_cost
                neighbour_node.prev = pos
                # inserted at end!
                # searchsortedfirst: returns index of the first value greater than or equal to x
                # rev=true reverses the lt function (isless by default)
                # returns end+1 if no element satisfies the condition
                # uses binary search
                # by=.. is also applied to the arg supplied
                # insert_index = searchsortedfirst(q, (nothing, new_cost), by=x->x[2], rev=true)

                # search linearly from the end of the arr (so we are closer to haskell solution)
                # insert_index(arr, x) = begin
                #     for i = size(arr, 1):-1:1
                #         (_, y) = arr[i]
                #         if x <= y
                #             return i + 1
                #         end

                #     end
                #     return 1
                # end
                insert_index = binsearch(q, new_cost)
                insert!(q, insert_index, (npos, new_cost))
            end
        end
    end
end

println(findPath!(grid, (1, 1), (size(grid[1], 1), size(grid, 1))))

# documentation on comprehensions is really bad, there's only one section
# the Multi-dimensional arrays page, but only a flat form is shown
# no nested examples, version to generate multidim arrays, shorthand syntax ....
# !_!_)!_
# would be esp. useful since the shorthand syntax apparently does sth. completely
# differnt in the context of a comprehension:
# can't use short-hand for the nested comprehension here (for y=1:5, x=1:5 transforms to:
# for y = 1:5
#   for x = 1:5
#   end
# end) (equivalent for y=1:5 for x=1:5 end end)
# even though they're equivalent ^, they make a difference used in a comprehension!??!
# it's only mentioned here: https://en.wikibooks.org/wiki/Introducing_Julia/Controlling_the_flow#Comprehensions
# no mention in the official docs
# => apparently the differnt behaviour is because column-major order is respected in the
# comprehension (but not outside) and only for the shorthand??!?!?
# we get multiple rows instead and end up with a Matrix instead of a nested Vector
five_x_grid = [[Node(
    # let block, could also use a begin block for multi line
    let level = (parse(Int, c) - 1 + xx + yy) % 9 + 1
        level
    # end, typemax(Int64), (-1, -1)) for c = l for xx = 0:4] for l = contents for yy = 0:4]
    # ^ the above actuall produces the wrong order with indices in a row resulting in:
    # (1, 1), (11, 1), (21, 1)
    # while cols will end up like so:
    # [(1, 1), ..], [(1, 11), ..], [(1, 21), ..],  .. [(1, 41), ..], [(1, 2), ..], [(1, 12), ..]
    # so we have to reverse the order of the for loops
    # it makes sense if you expand it to
    # for c = 1:10
    #   for xx = 0:4
    #     println(c, "+", xx*10)
    #   end
    # end
    # 1+0
    # 1+10
    # 1+20
    # 1+30
    # 1+40
    # 2+0
    # 2+10
    # ..
    # the xx/c and yy/l combinations are wrong so the wrong level is computed, but
    # the generated grid would've been fine otherwise
    end, typemax(Int64), (-1, -1)) for xx = 0:4 for c = l] for yy = 0:4 for l = contents]
println(findPath!(five_x_grid, (1, 1), (size(five_x_grid[1], 1), size(five_x_grid, 1))))


# ~0.5s on 3rd run with searchsortedfirst, ~5s with own impl to match Haskell/python
# ~0.4s with own binsearch
# python: 1.5s (with binsearch; without 6.7s)
# julia 0.4s with same binsearch as python (own impl); julia 5s (with stdlib bin search 0.5s)
# mikefarquhar's rust solution: 30ms
# toakushi's go solution: 184ms Dijkstra (A* 214ms)
# tpatetl's nodejs solution: 224s
# heyitsmattwade's nodejs solution: 1.5s (using a heap/prioqueue library)
# IT WAS ACTUALLY IMPOSSIBLE TO FIND A JS IMPLEMENTATION THAT DID NOT use
# A LIBRARY!!_)!!_)_!_!__!_!_!! HOW? what's the point of AoC then
# 3/5 used a lib for dijkstra which IS THE WHOLE CODING TASK!?!??!
# can't use binsearch with Haskell since the q is a list and iterating over the list
# is what's slow
# Haskell 728.1910445s
# F#: ~13s (with dotnet fsi d15.fsx command which apparently gets compiled)
# (^ also no binsearch, and using the same algo as Haskell, the ONLY difference is that
#  F# uses the mutable array whereas Haskell uses the immutable Data.Array implementation)
@time begin
    five_x_grid = [[Node(
        let level = (parse(Int, c) - 1 + xx + yy) % 9 + 1
            level
        end, typemax(Int64), (-1, -1)) for xx = 0:4 for c = l] for yy = 0:4 for l = contents]
    println(findPath!(five_x_grid, (1, 1), (size(five_x_grid[1], 1), size(five_x_grid, 1))))
end
@time begin
    five_x_grid = [[Node(
        let level = (parse(Int, c) - 1 + xx + yy) % 9 + 1
            level
        end, typemax(Int64), (-1, -1)) for xx = 0:4 for c = l] for yy = 0:4 for l = contents]
    println(findPath!(five_x_grid, (1, 1), (size(five_x_grid[1], 1), size(five_x_grid, 1))))
end