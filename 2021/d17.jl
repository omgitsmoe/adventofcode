# sample
contents = "target area: x=20..30, y=-10..-5"
# input
contents = "target area: x=206..250, y=-105..-57"

_, x_range_str, y_range_str = split(contents, "=")
target_x_min_str, target_x_max_str = split(x_range_str, "..")
# returns a range
target_x_max_str_end = findfirst(",", target_x_max_str)
# so we have to use .start
target_x_max_str = target_x_max_str[1:target_x_max_str_end.start - 1]
target_x = parse(Int, target_x_min_str):parse(Int, target_x_max_str)

target_y_min_str, target_y_max_str = split(y_range_str, "..")
target_y = parse(Int, target_y_min_str):parse(Int, target_y_max_str)

function sim_one_step(pos, v)
    x, y = pos
    vx, vy = v

    x += vx
    y += vy

    # x velocity changes TOWARDS 0 by 1
    if vx == 0
        dragx = 0
    elseif vx > 0
        dragx = -1
    else
        dragx = 1
    end

    vx += dragx

    # Y decreases downwards
    # gravity -> y velocity decreases by 1
    vy -= 1

    ((x, y), (vx, vy))
end

function is_beyond_target(start_pos, pos, v, target_x, target_y)
    result = false
    if start_pos[1] > target_x.start && start_pos[1] > target_x.stop
        # we're to the right of the target
        # if we're trying to shoot to the left of our starting pos we're never gonna hit the target
        # if our pos is smaller (left of) the target then we missed it
        result = result || pos[1] > start_pos[1] || pos[1] < target_x.start
    else
        result = result || pos[1] < start_pos[1] || pos[1] > target_x.stop
    end
    if start_pos[2] > target_y.start && start_pos[2] > target_y.stop
        # we're above (y is larger) the target
        # if our pos is smaller than the target we missed it (.start is the smaller one)
        # (since target_ is a unit range that alwasy goes from start:stop increasing
        #  from start to stop unless a step is specified)
        result = result || pos[2] < target_y.start
    else
        # we start below the target
        # if our velocity is not positive and we are still below the target then we missed
        result = result || v[2] <= 0 && pos[2] < target_y.start
    end

    result
end

function hits_target(start_pos, v, target_x, target_y)
    hit = false
    pos = start_pos
    while true
        pos, v = sim_one_step(pos, v)
        if (pos[1] in target_x && pos[2] in target_y)
            hit = true
            break
        end
        if is_beyond_target(start_pos, pos, v, target_x, target_y)
            break
        end
    end

    hit
end

# formula for uniform acceleration
# s(t) = 0.5*a*t^2+v0*t+s0
# s0 is fixed at 0,0 for us
# a = [-1,-1] in px/step2
# e.g. for first example s([-1, -1], 7, [7,2]) = 24.5
s(a, t, v0) = 0.5*a*t^2+v0*t

part1(target_min_y) = begin
    # (target_min_y here is the smaller number between target_min_y and target_max_y
    #  so -10..-5 it woul be -10)
    # if the veloctiy at y=0 (after goin back down) is -target_min_y we will just hit the
    # the end of the target range
    # if y-velocity starts at v it will reach y=0 with velocity -(v + 1)
    # since we have one more time step that accelerates towards the bottom
    # -(v+1)=min_y -> v = -min_y - 1
    v = -target_min_y - 1
    # n(n+1)/2 = sum of numbers 1 to n
    # ^ this sequence is the vel+neg accel in reverse
    # sum(1,2,3,4,5,6,7,8,9) in reverse is the steps with start vel y=9
    # since we have 9 + (9 - 1) + (8 - 1) where -1 is the accel due to gravity
    # y-vel=9 in the example -> 9(9+1)/2=45 which is the max height reached
    trunc(Int, v * (v + 1) / 2)
end


pos = (0, 0)

println("Part1: ", part1(target_y.start))

# valid velocities for example
# valid=[(23,-10);(25,-9);(27,-5);(29,-6);(22,-6);(21,-7);(9,0);(27,-7);(24,-5);
# (25,-7);(26,-6);(25,-5);(6,8);(11,-2);(20,-5);(29,-10);(6,3);(28,-7);
# (8,0);(30,-6);(29,-8);(20,-10);(6,7);(6,4);(6,1);(14,-4);(21,-6);
# (26,-10);(7,-1);(7,7);(8,-1);(21,-9);(6,2);(20,-7);(30,-10);(14,-3);
# (20,-8);(13,-2);(7,3);(28,-8);(29,-9);(15,-3);(22,-5);(26,-8);(25,-8);
# (25,-6);(15,-4);(9,-2);(15,-2);(12,-2);(28,-9);(12,-3);(24,-6);(23,-7);
# (25,-10);(7,8);(11,-3);(26,-7);(7,1);(23,-9);(6,0);(22,-10);(27,-6);
# (8,1);(22,-8);(13,-4);(7,6);(28,-6);(11,-4);(12,-4);(26,-9);(7,4);
# (24,-10);(23,-8);(30,-8);(7,0);(9,-1);(10,-1);(26,-5);(22,-9);(6,5);
# (7,5);(23,-6);(28,-10);(10,-2);(11,-1);(20,-9);(14,-2);(29,-7);(13,-3);
# (23,-5);(24,-8);(27,-9);(30,-7);(28,-5);(21,-10);(7,9);(6,6);(21,-5);
# (27,-10);(7,2);(30,-9);(21,-8);(22,-7);(24,-9);(20,-6);(6,9);(29,-5);
# (8,-2);(27,-8);(30,-5);(24,-7)]
# for v = valid
#     if !hits_target((0, 0), v, target_x, target_y)
#         println("Failed: ", v)
#     end
# end

# part2 ~15ms julia (10ms 3rd run)
# heyitsmattwade's js solution: 575ms
# lucashmsilva's js solution: 45ms
# senpal2000's python solution: 75ms
hits = []
# test all velocities from the min/max we can go in each direction without overshooting
# (technichally the upper y should be abs(target_y.start) - 1)
for vy = -abs(target_y.start):abs(target_y.start)
    for vx = 1:target_x.stop
        if hits_target((0, 0), (vx, vy), target_x, target_y)
            push!(hits, (vx, vy))
        end
    end
end
println("Part2: ", length(hits))