contents = open("d16.in") do f
    strip(read(f, String))
end

# enums are created with a macro
@enum PacketType begin
    sum=0
    product=1
    minimum=2
    maximum=3
    literal=4
    greater_than=5
    less_than=6
    equal=7
end

struct Packet
    type :: PacketType
    version :: Int
    payload :: Union{Integer, Vector{Packet}}
end

function hex_to_bin(s)
    result :: Vector{String} = []
    for c in s
        # convert single hex char into the int repr
        as_int = parse(Int, c, base = 16)
        # convert back as binary literal
        as_bin_literal = string(as_int, base = 2, pad = 4)
        push!(result, as_bin_literal)
    end
    
    join(result)
end

# no built-in way to specify the return type of a function that is a parameter in another
function skip_while(arr, pred::Function, start_idx::Integer)
    i = start_idx
    while true
        # can only specify the expected type when assigning it to a var in the function
        # but that gets checked at runtime apparently? wtf is this
        skip = pred(arr[i]) :: Bool
        if !skip
            break
        end

        i += 1
    end
    
    i
end

function parse_packet(s, start_idx)
    i = start_idx
    # first three bits version
    # range syntax packet_src[i:i+3] makes a copy
    # use SubString instead which is just a string view
    # println(s)
    # println(join(" " for _ = 1:i-1), "VVVTTT")
    version = parse(Int, SubString(s, i, i + 2), base = 2)
    # next three bits
    type_id = parse(Int, SubString(s, i + 3, i + 5), base = 2)
    i += 6
    packet_type = PacketType(type_id)
    # println(s)
    # println(join(" " for _ = 1:i-1), "^")
    # if does not open a new scope so we can assign packet and return it below
    if packet_type == literal
        # literal package
        i, payload = parse_literal(s, i)
    else
        # operator package
        # this can recurse
        i, payload = parse_operator(s, i)
    end

    (i, Packet(packet_type, version, payload))
end

function parse_literal(s, start_idx)
    i = start_idx
    # println(s)
    # println(join(" " for _ = 1:i-1), "^")
    # skip zero padding
    bits = []
    while true
        # first bit determines if we have more groups
        last_group = s[i] == '0'
        i += 1

        # 4 bits as one bit group
        bit_group = SubString(s, i, i + 3)
        # println("grp: ", bit_group, " last: ", last_group)
        push!(bits, bit_group)
        i += 4
        if last_group
            break
        end
    end
    
    num = parse(Int, join(bits), base = 2)
    # skip 3 extra 0 bits at the end
    # ^ very confusing this was apparently just for the example and not in general
    (i, num)
end

function parse_operator(s, start_idx)
    i = start_idx
    subpackets::Vector{Packet} = []
    # length type ID which signals which encoding for subpackage length is used
    if s[i] == '0'
        i += 1
        # next 15 bits are a number that represents the total length in
        # bits of the sub-packets
        subpacket_bit_length = parse(Int, SubString(s, i, i + 14), base = 2)
        i += 15

        end_index = i + subpacket_bit_length
        # parse all the bits of our subpackages
        # going to <= end_index would put us one beyond the end
        while i < end_index
            i, subpacket = parse_packet(s, i)
            push!(subpackets, subpacket)
        end
    else
        i += 1
        # next 11 bits are a number that represents the number of sub-packets
        subpacket_packet_length = parse(Int, SubString(s, i, i + 10), base = 2)
        i += 11

        # parse all the packets we expect
        for _ in 1:subpacket_packet_length
            i, subpacket = parse_packet(s, i)
            push!(subpackets, subpacket)
        end
    end

    (i, subpackets)
end

function evaluate_package(p::Packet)
    # call correct function based on package type
    if p.type == literal
        result = p.payload
    elseif p.type == sum
        result = sum_po(p)
    elseif p.type == product
        result = product_po(p)
    elseif p.type == minimum
        result = minimum_po(p)
    elseif p.type == maximum
        result = maximum_po(p)
    elseif p.type == greater_than
        result = greater_than_po(p)
    elseif p.type == less_than
        result = less_than_po(p)
    elseif p.type == equal
        result = equal_po(p)
    else
        error("unkown package type")
    end

    result
end

function sum_po(p::Packet)
    sum = 0
    for other in p.payload
        sum += evaluate_package(other)
    end

    sum
end

function product_po(p::Packet)
    result = 1
    for other in p.payload
        result *= evaluate_package(other)
    end

    result
end

minimum_po(p) = begin
    min = evaluate_package(p.payload[1])
    for subpacket in p.payload[2:end]
        value = evaluate_package(subpacket)
        if min > value
            min = value
        end
    end

    min
end

maximum_po(p) = begin
    max = evaluate_package(p.payload[1])
    for subpacket in p.payload[2:end]
        value = evaluate_package(subpacket)
        if max < value
            max = value
        end
    end

    max
end

# guaranteed to have two subpackets
greater_than_po(p) = let 
    # let on one line needs , as sep but on multi lines it doesn't
    a = evaluate_package(p.payload[1])
    b = evaluate_package(p.payload[2])
    
    a > b ? 1 : 0
end

# guaranteed to have two subpackets
less_than_po(p) = let 
    a = evaluate_package(p.payload[1])
    b = evaluate_package(p.payload[2])
    
    a < b ? 1 : 0
end

# guaranteed to have two subpackets
equal_po(p) = let 
    a = evaluate_package(p.payload[1])
    b = evaluate_package(p.payload[2])
    
    a == b ? 1 : 0
end


# literal test
# print(parse_packet(hex_to_bin("D2FE28"), 1))
# operator bitlength test
# print(parse_packet(hex_to_bin("38006F45291200"), 1))

@time begin
_, parsed_packet = parse_packet(hex_to_bin(contents), 1)
pq = [parsed_packet]
version_sum = 0
while length(pq) > 0
    packet = pop!(pq)
    # this would apparently create a new local and will assign to that
    # sum += packet.version
    # we have to use global to signal we want to use the global?!?!? IN A WHILE LOOP!>!>!
    global version_sum += packet.version
    if packet.type != literal
        # like python's .extend
        append!(pq, packet.payload)
    end
end
println("Part1: ", version_sum)
println("Part2: ", evaluate_package(parsed_packet))
end
# ^ 3rd run ~1ms; 1st 0.2s
# qualia91's haskell solution: 80ms ghci 45ms compiled
#
# NOTE: dotnet fsi -> F# interactive reads the code and executes it in real time
# DOES NOT COMPILE IT!! -> prob makes no difference since C# only gets "compiled" to the IL
# and is JIT-compiled on run
# created a console project and compiled it: the compiled version was somehow slower!?!??!
# and for other longer running days it was just as fast
# (also does not get faster on consecutive runs!)
#
# oddrationale's F# solution: 30ms (fsi) 80ms (compiled!?!??!)
# n_syn's python3 solution: 3ms (1st run as well)
# ^ but they're not building a packet "tree"
# farbfetzen's python3 solution: 2ms (1st run as well)
# ^ at least tracking subpacktes in lists
# travis-miller's go solution: ~1ms
# mikefarquhar's rust solution: 0.3ms

# got both parts first try (as in submitting)
# part1 need some debugging due to:
# # going to <= end_index would put us one beyond the end
# while i < end_index
# got part2 directly after fixing just compiler errors