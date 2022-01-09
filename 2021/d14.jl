
# open exectutes the function passed as first param and then closes the file
contents = split(strip(open(f -> read(f, String), "d14.in")), "\n")# |>
    # julia's pipe operator (can't start a line, has to be on same line or line end)
    # only works if the previous function the exact amount of args the next function needs
    # since partial appliactions don't return function (like in F# or Haskell)
    # strip([' ', '\n']) |> split("\n")
# indexing IS 1-based in julia -.-
polymer = contents[1]
rules = contents[3:end]

# no type declarations on globals !??! src_pair_to_pairs::Dict{...}
# but we can type the dict constructor:
# NOTE: type Any by default screwed me over multiple times, was still in the Haskell/F#
# mindset with static typing but heavy type inference, so I didn't specify the type
# and had a mix of array/str/char types in the dict
src_pair_to_pairs = Dict{Tuple{Char, Char}, Tuple{Tuple{Char, Char}, Tuple{Char, Char}}}()
for line in rules
    src, insert = split(line, " -> ")
    # string interpolation: "$var" or "$(expr)"
    # string concat is done using * (instead of + in python)
    # pair1 = "$(src[1])$insert"
    # insert[1] is a char 'c'
    # insert[1:1] is a string "c"
    pair1 = (src[1], insert[1])
    pair2 = (insert[1], src[2])
    src_pair_to_pairs[(src[1], src[2])] = (pair1, pair2)
end

#reduce(function (acc, x)
#    print(acc,x)
#end, rules, init=Dict())

# do block syntax: for functions that take a function as the first argument
# you can omit the function arg and write "do arg1, arg2, .." at the
# end followed by the function body and end
__src_pair_to_pairs2 = reduce(rules, init=Dict()) do acc, line
    src, insert = split(line, " -> ")
    # string interpolation: "$var" or "$(expr)"
    # string concat is done using * (instead of + in python)
    # TODO: changed above to tuple
    # TODO: str/char missmatch
    pair1 = "$(src[1])$insert"
    pair2 = "$insert$(src[2])"
    # last expression is returned
    # julia has multiple-dispatch and "method" calls are just called like normal functions
    # so e.g. merge decides which method to call based on all arguments
    # dict has => to constract pair objects, otherwise dict accepts (k, v) tuples
    # merge combines the two dicts (using value of last arg on conflict) returning the merged dict
    merge(acc, Dict((src[1], src[2]) => (pair1, pair2)))
end

function applyRules(
    polymer,
    rules::Dict{Tuple{Char, Char}, Tuple{Tuple{Char, Char}, Tuple{Char, Char}}},
    times::Number
)
    # create pair->count map
    pairs = Dict()
    for p in zip(polymer, polymer[2:end])
        pairs[p] = get(pairs, p, 0) + 1
    end

    for _ = 1:times
        # # imperative version (>20x faster than below -> 2ms)
        # # need to use separate dict otherwise we might add more pairs that belong
        # # to the next step which then might get used for adding ohter pairs...
        # new_pairs = copy(pairs)
        # # unlike in python julia iterates over the key-value pairs in for .. in dict
        # for (src_pair, (pair1, pair2)) in rules
        #     if !haskey(pairs, src_pair)
        #         continue
        #     end

        #     # use old dict for count lookup
        #     count = pairs[src_pair]
        #     new_pairs[src_pair] -= count
        #     new_pairs[pair1] = get(new_pairs, pair1, 0) + count
        #     new_pairs[pair2] = get(new_pairs, pair2, 0) + count
        # end

        # pairs = new_pairs
        # functional version (with this part2 is >20x slower -> 50ms)
        pairs = reduce(pairs, init=pairs) do acc, (src_pair, count)
            if !haskey(rules, src_pair)
                acc
            else
                pair1, pair2 = rules[src_pair]

                # src pair might be one of the resulting pairs, so we can't pass it
                # to the dict constructor since one of the keys might get overwritten
                # or we could have the same resulting pairs
                #   Dict(pair1 => get(acc, pair1, 0) + count,
                #        pair2 => get(acc, pair2, 0) + count,
                #        src_pair => acc[src_pair] - count))
                # there's no functional change/insertWith for dicts....
                # (only reason why i even tried ^^)
                # mergewith: duplicate keys get merged using function passed as first arg
                # mergewith just uses the non-functional version mergewith! in the impl..

                # just replacing functional merge below with this makes it 20x faster
                acc[src_pair] -= count
                acc[pair1] = get(acc, pair1, 0) + count
                acc[pair2] = get(acc, pair2, 0) + count
                acc
                # mergewith(+, acc,
                #     Dict(pair1 => count),
                #     Dict(pair2 => count),
                #     Dict(src_pair => -count))
            end
        end
    end

    pairs
end

function countChars(pairs)
    char_to_count = Dict()
    for (pair, count) in pairs
        for c in pair
            char_to_count[c] = get(char_to_count, c, 0) + count
        end
    end

    char_to_count
end

# corrects counts since one char gets reused between 2 pairs
function correctCounts(char_to_count)
    # no map method for dict, but there's map! which is the imperative version
    # map!(f, values(dict))
    char_to_count = map!(values(char_to_count)) do count
        # can't use int truncation, since it produces: InexactError
        # we have to round explicitly -> uses rounding to nearest even by default
        # so use trunc or specify diff rounding mode: RoundNearestTiesAway
        # round(Int64, count / 2, RoundNearestTiesAway)
        trunc(Int64, count / 2 + 0.5)
    end
end

# pairs1 = applyRules(polymer, src_pair_to_pairs, 10)
# char_to_count1 = correctCounts(countChars(pairs1))
# min1 = minimum(values(char_to_count1))
# max1 = maximum(values(char_to_count1))
# println("Part1: ", max1 - min1)

# ~0.7s for just part2 vs F# (with compilation time 2.5s) (without 27ms)
# (both scripts use almost the exact same algo, julia version is a little more imperative
#  which should make it a little faster)
# Julia without JIT compile time:
# the second run is much faster: 0.05s -> 50ms
# python and julia are fastest between python, julia and F#
# python: 2ms julia: 2ms (applyrules functional reduce 50ms) F#: 27ms
@time begin
    pairs2 = applyRules(polymer, src_pair_to_pairs, 40)
    char_to_count2 = correctCounts(countChars(pairs2))
    min2 = minimum(values(char_to_count2))
    max2 = maximum(values(char_to_count2))
    println("Part2: ", max2 - min2)
end
@time begin
    pairs2 = applyRules(polymer, src_pair_to_pairs, 40)
    char_to_count2 = correctCounts(countChars(pairs2))
    min2 = minimum(values(char_to_count2))
    max2 = maximum(values(char_to_count2))
    println("Part2: ", max2 - min2)
end