import std/sugar, std/strutils, std/sequtils, std/algorithm

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
    Mapping = object
        srcKind: IdKind
        destKind: IdKind
        # ranges are only for passing  single values and then it's checked whether
        # that value is in range
        # and slice can only be made from collections -> can't use it here
        src_min: int
        src_max: int
        offset: int

proc mapKind(self: Number, mappings: seq[Mapping]): Number =
    result = self

    # look for a mapping, otherwise the id stays the same
    for m in mappings:
        if self.kind == m.srcKind and (self.id >= m.src_min and self.id <= m.src_max):
            result.id += m.offset
            break

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
    mappings: seq[Mapping] = @[]

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
            offset = dest_start - source_start

        mappings.add(Mapping(srcKind: srcKind, destKind: destKind,
            src_min: source_start, src_max: source_start + range_len - 1,
            offset: offset))

echo "built mapping"

proc locFromSeed(id: int): int =
    var num = Number(kind: IdKind.Seed, id: id)
    while num.kind != IdKind.Location:
        num = num.mapKind(mappings)

    return num.id

var locs: seq[int] = @[]

for id in seeds:
    locs.add(locFromSeed(id))

locs.sort()
echo "Part1: Closest location $#" % [$locs[0]]

type
    SeedRange = object
        seed_min: int
        seed_max: int
        id_kind: IdKind
        offset: int

proc id_min(self: SeedRange): int =
    self.seed_min + self.offset

proc id_max(self: SeedRange): int =
    self.seed_max + self.offset

# we keep the original seed range but continously update the offset
# in this proc we split off the parts of the range `self` that don't fall
# into `mapping` keeping the offsets (and not chaning the id kind,
# since they should only be 'counted' as mapped once all mappings
# have been visited
# then we adjust the offset for the part that falls in the mapping
# and return all the splits
proc splitRange(self: SeedRange, mapping: Mapping): seq[SeedRange] =
    if self.id_max() < mapping.src_min or self.id_min() > mapping.src_max:
        # not included in mapping -> id remains
        # don't change kind yet in case there are other mappings that fit
        return @[self]

    var
        new_seed_min = self.seed_min
        new_seed_max = self.seed_max

    if self.id_min() < mapping.src_min:
        # separate sequence that's outside of the mapping on the lower side
        # don't change kind yet in case there are other mappings that fit
        var tmp = self
        # get the new size of the part before the mapping range
        # and add it to original min
        # subtract one since we don't want mapping.src_min included
        tmp.seed_max = tmp.seed_min + (mapping.src_min - self.id_min()) - 1
        result.add(tmp)
        # ranges that fall into it should now start after the split off segment
        new_seed_min = tmp.seed_max + 1

    if self.id_max() > mapping.src_max:
        # separate sequence that's outside of the mapping on the higher side
        # don't change kind yet in case there are other mappings that fit
        var tmp = self
        # get the new size of the segment after the range mapping
        # then subtract it from the seed_max to get the new start
        # re-add one since we don't want mapping.src_max included
        tmp.seed_min = tmp.seed_max - (self.id_max() - mapping.src_max) + 1
        result.add(tmp)
        # ranges that fall into it should now end at the segment start
        new_seed_max = tmp.seed_min - 1

    # at least one part must've fallen into the range otherwise we'd have
    # returned early
    var tmp = self
    # ranges can't overlap so we can set the new kind here
    # which will also prevent mapping this segment again
    tmp.id_kind = mapping.destKind
    tmp.seed_min = new_seed_min
    tmp.seed_max = new_seed_max
    # adjust new offset, since this segment was already mapped
    tmp.offset += mapping.offset
    result.add(tmp)

var
    idx = 0
    seed_offsets: seq[SeedRange] = @[]

# there's no seq.pop..
while idx < len(seeds):
    let
        seed_min = seeds[idx]
        range_len = seeds[idx + 1]
        seed_max = seed_min + range_len - 1
    idx += 2
    seed_offsets.add(SeedRange(
        seed_min: seed_min, seed_max: seed_max, id_kind: IdKind.Seed))

let kinds = @[Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location]
# do all the transformations on the __ranges__ (instead of on individual items)
# Seed->Soil, Soil->..
for i, kind in pairs(kinds):
    if kind == Location:
        break

    for mapping in mappings:
        if mapping.srcKind != kind:
            continue

        # there's still no seq.pop...
        # we need to build them up in a separate array, since we're iterating
        # over the original and we can't just pop items till they're done, since
        # some ranges might not be mapped at all
        var newd_seed_offsets: seq[SeedRange] = @[]
        for sr in seed_offsets:
            if sr.id_kind != kind:
                # already mapped to destination IdKind, so append without splitting
                newd_seed_offsets &= @[sr]
                continue
            else:
                let splits = sr.splitRange(mapping)
                # concat new split up ranges
                newd_seed_offsets &= splits

        seed_offsets = newd_seed_offsets

    # go through the ranges again and set the new kind, so ranges which didn't
    # fall into a mapping are marked as 'mapped'
    # use mitems, so we can modify them
    for s in seed_offsets.mitems():
        s.id_kind = kinds[i + 1]

let
    min_offset = seed_offsets.foldl(
        if (b.seed_min + b.offset) < (a.seed_min + a.offset): b else: a, 
        seed_offsets[0])
    min_loc = min_offset.seed_min + min_offset.offset
echo "Part2: Closest location $#" % [$min_loc]

