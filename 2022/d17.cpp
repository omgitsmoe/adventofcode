#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <charconv>
#include <algorithm>
#include <array>

// chamber is seven units wide (as in between the left and right wall are seven units of space)
constexpr int RIGHT_WALL_X = 8;
struct Point { long long y, x; };

inline bool comparePoints(Point a, Point b) {
    return (a.y < b.y) || (a.y == b.y && a.x < b.x);
}

// decltype gets declared type of function pointer (since we pass & - instead we could use * after decltype)
// bool collides(const std::set<Point, decltype(&comparePoints)>& rocks, Point p, const std::vector<Point>& shape) {
// parens around &highest_y since we'd otherwise get an array of refs
// bool collides(const long long (&highest_y)[7], Point p, const std::vector<Point>& shape) {
template<typename T>
bool collides(const T& highest_y, Point p, const std::vector<Point>& shape) {
    for (auto offset : shape) {
        Point po = { p.y + offset.y, p.x + offset.x };
        if (po.x <= 0) {
            // collides with left wall
            return true;
        }
        if (po.x >= RIGHT_WALL_X) {
            // collides with right wall
            return true;
        }

        // check for collision with already fallen rock
        // we know po.x is in bounds -> -1 since we don't have an element for the left wall
        // TODO allowed to use >= isntead of ==?
        if (highest_y[po.x - 1] >= po.y) {
            // collides with resting rock/floor
            return true;
        }
        // if (highest_y[po.x - 1] > po.y) {
        //     std::cout << "OOOPSSIE" << std::endl;
        // }
    }

    return false;
}

template<typename T, typename C>
bool collides(const std::set<T, C>& occupied, Point p, const std::vector<Point>& shape) {
    for (auto offset : shape) {
        Point po = { p.y + offset.y, p.x + offset.x };
        if (po.y <= 0) {
            // colliding with floor
            return true;
        }
        if (po.x <= 0) {
            // collides with left wall
            return true;
        }
        if (po.x >= RIGHT_WALL_X) {
            // collides with right wall
            return true;
        }

        // check for collision with already fallen rock
        if (occupied.find(po) != occupied.end()) {
            // collides with resting rock/floor
            return true;
        }
    }

    return false;
}

// does not include the occupied set
struct SimState {
    long long rock_idx;
    int jet_idx;
    long long max_y;
};

SimState do_steps(
    const SimState state,
    const long long max_rocks,
    const std::string& jets,
    const std::vector<std::vector<Point>>& shapes,
    std::set<Point, decltype(&comparePoints)>& in_out_occupied,
    bool stop_at_jet_wrap = false
) {
    long long rock_idx = state.rock_idx;
    long long tallest_y = state.max_y;
    int jet_idx = state.jet_idx;

    const int shape_height[5] = {1, 3, 3, 4, 2};
    const int shape_max = shapes.size();
    int shape_idx = static_cast<int>(rock_idx % static_cast<long long>(shape_max));
    // x=0 as left wall
    // spawn 2 units right from the left wall (as in 2 units of space between the wall and left edge,
    // not wall.x + 2)
    constexpr int spawn_x = 3;
#if MANUAL_CYCLE_DETECTION
    long long prev_tallest_y = tallest_y;
#endif
    const int jets_max = jets.size();
    // int jet_idx = rock_idx % jets_max;
    int prev_jet_idx = jet_idx;
    for (; rock_idx < max_rocks; ++rock_idx) {
        const auto& shape_offsets = shapes[shape_idx];
        // __BOTTOM__ edge is three units above the highest rock/floor
        // (as in 3 units of space between floor/highest rock and the bottom edge)
        Point p = { tallest_y + 3 + shape_height[shape_idx], spawn_x };
        // jet idx before simulating next rock
        // -> for detecting wrapping to start of list
        prev_jet_idx = jet_idx;

        while (true) {
            // jet, then fall down
            // -> hitting wall/rock/floor -> doesn't occur
            // if a downwards movement would move into a rock/floor -> stop
            auto jet = jets[jet_idx];
            jet_idx = (jet_idx + 1) % jets_max;
            Point pushed;
            if (jet == '>') {
                pushed = { p.y, p.x + 1 };
            } else {
                pushed = { p.y, p.x - 1 };
            }

            if (!collides(in_out_occupied, pushed, shape_offsets)) {
                // no collision so push occurs
                p = pushed;
            }
            // std::cout << "After push at y=" << p.y << " x=" << p.x << std::endl;

            Point downward = { p.y - 1, p.x };
            if (collides(in_out_occupied, downward, shape_offsets)) {
                // std::cout << "resing at y=" << p.y << " x=" << p.x << std::endl;
                // add rocks
                for (auto offset : shape_offsets) {
                    in_out_occupied.insert({ p.y + offset.y, p.x + offset.x });
                }
                // all shapes have the top row occupied, so we can just that y
                if (p.y > tallest_y) tallest_y = p.y;

                // continue with next rock
                break;
            } else {
                p = downward;
            }
        }

#if MANUAL_CYCLE_DETECTION
        // started from the beginning of our jet list
        if (prev_jet_idx > jet_idx) {
            // difference in height will start repeating
            long long dh = tallest_y - prev_tallest_y;
            std::cout << "rock_idx " << rock_idx << " h " << tallest_y << " dh " <<
                dh << " jet_idx " << jet_idx << " shape "
                << (shape_idx + 1) % shape_max << std::endl;
            // as will all the highest_y
            // for (auto h : highest_y) {
            //     std::cout << h << ", ";
            // }
            // std::cout << std::endl;
            prev_tallest_y = tallest_y;
            prev_jet_idx = jet_idx;
        }
#endif
        if (stop_at_jet_wrap && (prev_jet_idx > jet_idx)) {
            break;
        }

        shape_idx = (shape_idx + 1) % shape_max;
    }
    
    return { rock_idx, jet_idx, tallest_y };
}

int main(int argc, const char** argv) {
    // 2022 for part1
    // 1000000000000 for part2
    // constexpr long long max_rocks = 2022; //1000000000000;
    long long max_rocks;
    if (argc < 2) {
        max_rocks = 2022;
    } else {
        // look for \0
        const char* rocksnum_end = argv[1];
        while (*rocksnum_end) { rocksnum_end++; };
        std::from_chars(argv[1], rocksnum_end, max_rocks);
    }

    std::ifstream infile("d17.in");
    std::string jets;
    getline(infile, jets);

    std::vector<std::vector<Point>> shapes;
    // y grows upwards
    // top-left as origin for shape/rock
    shapes.emplace_back(std::vector<Point>{ { 0, 0 }, { 0, 1 }, { 0, 2 }, { 0, 3 } });
    shapes.emplace_back(std::vector<Point>{ { 0, 1 }, { -1, 0 }, { -1, 1 }, { -1, 2 }, { -2, 1 } });
    shapes.emplace_back(std::vector<Point>{ { 0, 2 }, { -1, 2 }, { -2, 0 }, { -2, 1 }, { -2, 2 } });
    shapes.emplace_back(std::vector<Point>{ { 0, 0 }, { -1, 0 }, { -2, 0 }, { -3, 0 } });
    shapes.emplace_back(std::vector<Point>{ { 0, 0 }, { 0, 1 }, { -1, 0 }, { -1, 1 } });

    {
        const int shape_height[5] = {1, 3, 3, 4, 2};
        // NOTE: !IMPORTANT! does not work for part2 with my real input, so had to revert back to using a
        // set of points
        long long highest_y[7] = {0, 0, 0, 0, 0, 0, 0}; 

        int shape_idx = 0;
        const int shape_max = shapes.size();
        // x=0 as left wall
        // spawn 2 units right from the left wall (as in 2 units of space between the wall and left edge,
        // not wall.x + 2)
        constexpr int spawn_x = 3;
        // use y=0 as floor;
        long long tallest_y = 0;
        const int jets_max = jets.size();
        // doesn't reset with every rock!
        int jet_idx = 0;
        // std::set<Point, decltype(&comparePoints)> rocks(comparePoints);
        for (long long i = 0; i < max_rocks; ++i) {
            const auto& shape_offsets = shapes[shape_idx];
            // __BOTTOM__ edge is three units above the highest rock/floor
            // (as in 3 units of space between floor/highest rock and the bottom edge)
            Point p = { tallest_y + 3 + shape_height[shape_idx], spawn_x };
            while (true) {
                // jet, then fall down
                // -> hitting wall/rock/floor -> doesn't occur
                // if a downwards movement would move into a rock/floor -> stop
                auto jet = jets[jet_idx];
                jet_idx = (jet_idx + 1) % jets_max;
                Point pushed;
                if (jet == '>') {
                    pushed = { p.y, p.x + 1 };
                } else {
                    pushed = { p.y, p.x - 1 };
                }

                if (!collides(highest_y, pushed, shape_offsets)) {
                    // no collision so push occurs
                    p = pushed;
                }
                // std::cout << "After push at y=" << p.y << " x=" << p.x << std::endl;

                Point downward = { p.y - 1, p.x };
                if (collides(highest_y, downward, shape_offsets)) {
                    // std::cout << "resing at y=" << p.y << " x=" << p.x << std::endl;
                    // add rocks
                    for (auto offset : shape_offsets) {
                        int hx = p.x + offset.x - 1; // no element for left wall so -1
                        long long hy = p.y + offset.y;
                        if (highest_y[hx] < hy) highest_y[hx] = hy;
                    }
                    // all shapes have the top row occupied, so we can just that y
                    if (p.y > tallest_y) tallest_y = p.y;

#if PRINT_CAVE // does not work without tracking all points
                    for (int y = tallest_y; y > 0; --y) {
                        std::cout << "|";
                        for (int x = 1; x < RIGHT_WALL_X; ++x) {
                            if (rocks.find(Point{ y, x }) != rocks.end()) {
                                std::cout << "#";
                            } else {
                                std::cout << ".";
                            }
                        }
                        std::cout << "|" << std::endl;
                    }
                    std::cout << "+-------+" << std::endl << std::endl;
#endif

                    // continue with next rock
                    break;
                } else {
                    p = downward;
                }
            }

            shape_idx = (shape_idx + 1) % shape_max;
        }

        std::cout << "Part1: Tallest rock is at y=" << tallest_y << std::endl;
    }

#if 0
    std::set<std::tuple<long long, long long>> patts;
    std::tuple<long long, long long> longest;
    for (size_t i = 0; i < dhs.size(); ++i) {
        auto start = dhs.begin() + i;
        auto next = std::find(dhs.begin() + i + 1, dhs.end(), *start);
        while (next != dhs.end()) {
            auto other = next;
            int matching = 0;
            long long sum = 0;
            auto cur = dhs.begin() + i;
            bool reached_other = false;
            while ((*other == *cur)) {
                if (cur == next) {
                    // reached other pattern's start
                    // std::cout << "REPETITION!!! at " << (cur - dhs.begin()) * 5 << std::endl;
                    reached_other = true;
                    break;
                }
                if (cur == dhs.end()) std::cout << "End patt " << i << std::endl;
                sum += *cur;
                other++;
                cur++;
                matching++;
            }
            if (reached_other) {
                auto ins = std::make_tuple(matching, sum);
                if (patts.find(ins) != patts.end()) {
                    if (matching > std::get<0>(longest)) {
                        std::cout << "new longest repeating pattern -> i " << i << " Matching " << matching << " dh " << sum << std::endl;
                        longest = ins;
                    }
                } else {
                    patts.emplace(ins);
                }

                break;
            }
            // reset
            next = std::find(next + 1, dhs.end(), *start);
        }
    }
#endif

    // below prints out delta height at jets wrapping points which then can be used to figure
    // out the cycle manually
#if MANUAL_CYCLE_DETECTION
    // part2
    // track height differences after one round of shapes is done:
    // i 0 h 0 dh 0
    // i 5 h 9 dh 9
    // i 10 h 17 dh 8
    // i 15 h 25 dh 8
    // i 20 h 36 dh 11
    // i 25 h 43 dh 7
    // i 30 h 51 dh 8
    // i 35 h 60 dh 9
    // i 40 h 66 dh 6
    // i 45 h 72 dh 6
    // i 50 h 78 dh 6
    // i 55 h 89 dh 11
    // i 60 h 96 dh 7
    // i 65 h 104 dh 8
    // i 70 h 113 dh 9
    // i 75 h 119 dh 6
    // i 80 h 125 dh 6
    // i 85 h 131 dh 6
    // ...
    // height diffs of 11, 7, 8, 9, 6, 6, 6
    // starts repeating after h 25 at rock 15
    // so 53 h every 35 rocks
    // >>> (1000000000000 - 15) / 35
    // 28571428571.0
    // ^ batches of 35 rocks
    // -> 25 starting height at 15 rocks + batches of 35 * 53 height per batch:
    // >>> 25 + 28571428571 * 53
    // 1514285714288
    //
    // NOTE: this is enough for the example, but infeasable/hard to track for the real input
    // -> detect jet wrapping point (starting at beginning) and track diff in height:
    // rock_idx 1739 h 2754 dh 2754 jet_idx 6 shape 0
    // rock_idx 3484 h 5521 dh 2767 jet_idx 6 shape 0
    // rock_idx 5229 h 8288 dh 2767 jet_idx 6 shape 0
    // rock_idx 6974 h 11055 dh 2767 jet_idx 6 shape 0
    // rock_idx 8719 h 13822 dh 2767 jet_idx 6 shape 0
    // batch_size = 3484 - 1739 = 1745
    // remaining_steps = (1000000000000 - 1739) % 1745 = 1016
    // doesn't divide evenly so simulate to the point where it starts repeating (1739)
    // then add the rocks we can:
    // add (1000000000000 - 1739 - 1016) = 999999997245 rocks
    // add height = 999999997245 / 1745 * 2767 = 1585673348067
    // at the height we can compute and then simulate the remaining steps

    {
        std::set<Point, decltype(&comparePoints)> occupied(comparePoints);
        SimState state = {0};
#if EXAMPLE // example with jets wrapping
        // rock_idx 14 h 25 dh 8 jet_idx 2
        // rock_idx 22 h 42 dh 17 jet_idx 1
        // rock_idx 29 h 51 dh 9 jet_idx 5
        // rock_idx 36 h 63 dh 12 jet_idx 3
        // rock_idx 43 h 72 dh 9 jet_idx 2
        // rock_idx 49 h 78 dh 6 jet_idx 2
        const int start_repeating_idx = 14;
        state = do_steps(state, start_repeating_idx + 1, jets, shapes);
        const int batch_size = 35; // 49 - 14
        const int dh_per_batch = 53; // 17 + 9 + 12 + 9 + 6
        const long long remaining_steps = (1000000000000LL - start_repeating_idx) % batch_size;
        std::cout << "remaining steps " << remaining_steps << std::endl;
        const long long computed_rocks = 1000000000000LL - start_repeating_idx - remaining_steps;
        const long long delta_height = computed_rocks / batch_size * dh_per_batch;

        // at the start of a cycle top rows of the rocks will also match
        // so we just add the delta_height to each point
        std::set<Point, decltype(&comparePoints)> occupied_dh(comparePoints);
        for (auto p : occupied) {
            occupied_dh.emplace(Point{ p.y + delta_height, p.x });
        }
        state.max_y += delta_height;

        const long long remaining_start_idx = start_repeating_idx + computed_rocks + 1;
        state.rock_idx = remaining_start_idx;
        // !IMPORTANT! use same jet_idx (2)
        state = do_steps(state, 1000000000000LL, jets, shapes);

#else
        const int start_repeating_idx = 1739;
#if 1
        state = do_steps(state, start_repeating_idx + 1, jets, shapes, occupied);
        const int batch_size = 3484 - start_repeating_idx;
        const int dh_per_batch = 2767;
        const long long remaining_rocks = 1000000000000LL - start_repeating_idx;
        // (if it) doesn't divide evenly
        const long long need_simulation = remaining_rocks % batch_size;
        const long long computed_rocks = remaining_rocks - need_simulation;
        std::cout << "computed rocks " << computed_rocks << " need sim " << need_simulation << std::endl;
        const long long delta_height = (computed_rocks / batch_size) * dh_per_batch;
        std::cout << "delta height " << delta_height << std::endl;

        // at the start of a cycle top rows of the rocks will also match
        // so we just add the delta_height to each point
        std::set<Point, decltype(&comparePoints)> occupied_dh(comparePoints);
        for (auto p : occupied) {
            occupied_dh.emplace(Point{ p.y + delta_height, p.x });
        }
        state.max_y += delta_height;

        const long long remaining_start_idx = start_repeating_idx + computed_rocks + 1;
        state.rock_idx = remaining_start_idx;
        // !IMPORTANT! use same jet_idx (6)
        state = do_steps(state, 1000000000000LL, jets, shapes, occupied_dh);
#else
        state.max_y = do_steps(state, 20000, jets, shapes, occupied);
#endif
        std::cout << "Part2: Tower height " << state.max_y << std::endl;
    }
#endif
#endif


    // detecting cycles automatically
    // (adapted from https://github.com/dclamage/AOC2022/blob/main/day17/src/main.rs)
    // 1. simulate a certain amount of rocks, where we suspect the cycle will occur in
    const int SHAPES_SIM = 10000;
    // 2. keep track of height deltas that were reached at each step (or batch of steps,
    // for one jet wrap-around, in our case)
    // dh, rock_idx, jet_idx
    std::vector<std::tuple<long long, long long, int>> height_deltas;

    std::set<Point, decltype(&comparePoints)> occupied(comparePoints);
    SimState state = {0};
    while (state.rock_idx < SHAPES_SIM) {
        // NOTE: !IMPORTANT! pass INT_MAX as max_rocks otherwise we might stop before reaching a
        // jets wrapping point when SHAPES_SIM isn't at the exact boundary
        SimState new_state = do_steps(state, INT_MAX, jets, shapes, occupied, true);
        long long dh = new_state.max_y - state.max_y;
        height_deltas.emplace_back(std::make_tuple(dh, new_state.rock_idx, new_state.jet_idx));
        state = new_state;
    }

    // 3. try to find a cycle by looking at pattern lengths of 1 to half of the buffer
    // -> if it repeats till end of buffer => found cycle
    // NOTE: it takes time for the cycle to start, so we need to skip a few entries
    // (low since we only record delta heights when jets wrap - compared to tracking dh of every rock)
    const long long PATTERN_SKIP = 3;
    if (PATTERN_SKIP > (static_cast<int>(height_deltas.size()) - 3)) {
        std::cout << "Not enough batches (" << height_deltas.size() << ") for skipped patterns" << std::endl;
        exit(1);
    }
    const size_t skipped_size = height_deltas.size() - PATTERN_SKIP;
    const auto height_deltas_start = height_deltas.begin() + PATTERN_SKIP;
    int found_pattern_len = 0;
    for (int pattern_len = 1; pattern_len <= (skipped_size / 2); ++pattern_len) {
        // if we had slices:
        // pattern = height_deltas_start[0..pattern_len];
        bool found = true;
        // compare pattern to rest of the height deltas
        for (int i = 0; i < (skipped_size - pattern_len); ++i) {
            // compare current position in pattern (% to wrap around) to each item after the pattern
            // (+ pattern_len) if any of them don't match -> no repeating pattern
            auto after_patt = *(height_deltas_start + (i + pattern_len));
            auto in_patt = *(height_deltas_start + (i % pattern_len));
            if (std::get<0>(after_patt) != std::get<0>(in_patt)) {
                found = false;
                break;
            }
        }

        // pattern repeated till end of buffer
        if (found) {
            found_pattern_len = pattern_len;
            // break at shortest pattern
            break;
        }
    }

    std::cout << "Found cycle of len " << found_pattern_len << std::endl;

    // 4. calculate total dh of the cycle as well as batch size and jet_idx
    long long dh_per_batch = 0;
    long long batch_size = 0;
    // not needed (since the first simming step will have it in state), but should make sure they match
    // const int jet_idx = std::get<2>(*height_deltas_start);
    const long long start_repeating_idx = std::get<1>(*height_deltas_start);
    for (int i = 0; i < found_pattern_len; ++i) {
        const auto& batch = *(height_deltas_start + i);
        dh_per_batch += std::get<0>(batch);
        // rocks in batch
        batch_size += std::get<1>(batch) - std::get<1>(*(height_deltas_start + i - 1));
    }

    // reset
    state = {0};
    occupied.clear();
    // simulate to the point where it starts repeating
    state = do_steps(state, start_repeating_idx + 1, jets, shapes, occupied);
    const long long remaining_rocks = 1000000000000LL - start_repeating_idx;
    // (if it) doesn't divide evenly
    const long long need_simulation = remaining_rocks % batch_size;
    const long long computed_rocks = remaining_rocks - need_simulation;
    std::cout << "computed rocks " << computed_rocks << " need sim " << need_simulation << std::endl;
    const long long delta_height = (computed_rocks / batch_size) * dh_per_batch;
    std::cout << "total delta height " << delta_height << std::endl;

    // at the start of a cycle top rows of the rocks will also match
    // so we just add the delta_height to each point
    std::set<Point, decltype(&comparePoints)> occupied_dh(comparePoints);
    for (auto p : occupied) {
        occupied_dh.emplace(Point{ p.y + delta_height, p.x });
    }
    state.max_y += delta_height;

    const long long remaining_start_idx = start_repeating_idx + computed_rocks + 1;
    state.rock_idx = remaining_start_idx;
    std::cout << "Simming remaining rocks" << std::endl;
    state = do_steps(state, 1000000000000LL, jets, shapes, occupied_dh);
    std::cout << "Part2: Tower height " << state.max_y << std::endl;
}
