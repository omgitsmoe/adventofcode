import std/tables, std/hashes, std/sugar, std/strutils, std/sequtils, std/algorithm

let
    input_file = "d05_input.txt"
    # input_file = "d05_example.txt"
    input = (readFile input_file).split('\n')
    seed_line = input[0]
    input_rest = input[1..^1] # end of slice with ^1 has to be explicit

type
    # only non-ambigious can be specified without using `IdKind.`
    # otherwise just using Seed, Soil, etc. would work
    IdKind {.pure.} = enum
        Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location
    Number = object
        kind: IdKind
        id: int

# needed to support hashing of a custom type
proc hash(self: Number): Hash =
    # operator `!&` to mix hash values (uses existing hash impls)
    # enum also needs `hash` defined, so just use the int representation with `ord`
    result = ord(self.kind) !& self.id
    # finish hash computation
    result = !$result

proc mapKind(self: Number, numberMap: Table[Number, Number]): Number =
    try:
        result = numberMap[self]
    except KeyError:
        result = self
        case result.kind
        of IdKind.Seed:
            result.kind = IdKind.Soil
        of IdKind.Soil:
            result.kind = IdKind.Fertilizer
        of IdKind.Fertilizer:
            result.kind = IdKind.Water
        of IdKind.Water:
            result.kind = IdKind.Light
        of IdKind.Light:
            result.kind = IdKind.Temperature
        of IdKind.Temperature:
            result.kind = IdKind.Humidity
        of IdKind.Humidity:
            result.kind = IdKind.Location
        of IdKind.Location:
            assert false, "Location should not be mapped"

let seeds = seed_line
    .split(' ')
    .filter(d => isDigit d[0])
    .map(s => parseInt s)

var
    # from is a keyword...
    srcKind = IdKind.Seed
    destKind = IdKind.Soil
    numberMap = initTable[Number, Number]()

for line in input_rest:
    if len(line) == 0:
        continue

    if line.startsWith("seed-to-soil"):
        srcKind = IdKind.Seed
        destKind = IdKind.Soil
    elif line.startsWith("soil-to-fertilizer"):
        srcKind = IdKind.Soil
        destKind = IdKind.Fertilizer
    elif line.startsWith("fertilizer-to-water"):
        srcKind = IdKind.Fertilizer
        destKind = IdKind.Water
    elif line.startsWith("water-to-light"):
        srcKind = IdKind.Water
        destKind = IdKind.Light
    elif line.startsWith("light-to-temperature"):
        srcKind = IdKind.Light
        destKind = IdKind.Temperature
    elif line.startsWith("temperature-to-humidity"):
        srcKind = IdKind.Temperature
        destKind = IdKind.Humidity
    elif line.startsWith("humidity-to-location"):
        srcKind = IdKind.Humidity
        destKind = IdKind.Location
    else:
        let
            nums = line.split(' ').map(d => parseInt d)
            dest_start = nums[0]
            source_start = nums[1]
            range_len = nums[2]

        echo "Rangelen $#" % [$range_len]
        for i in 0..<range_len:
            let
                source = Number(kind: srcKind, id: source_start + i)
                dest = Number(kind: destKind, id: dest_start + i)
            numberMap[source] = dest

echo "built mapping"

var locs: seq[int] = @[]

for id in seeds:
    var num = Number(kind: IdKind.Seed, id: id)
    while num.kind != IdKind.Location:
        num = num.mapKind(numberMap)
    locs.add(num.id)

locs.sort()
echo "Part1: Closest location $#" % [$locs[0]]
