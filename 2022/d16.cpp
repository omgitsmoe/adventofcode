#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <charconv>
#include <set>
#include <deque>
#include <tuple>
#include <algorithm>
#include <chrono>

#include "utils.cpp"

using BitMapType = long long;

struct Valve {
    std::string key;
    int flow_rate;
    bool open = false;
    std::vector<std::string> tunnels;
    std::map<std::string, int> path_costs;
    int valve_idx;
};

struct Path {
    int path_cost;
    std::string key;
};

template<typename ValveTrackerType>
struct SearchStateMem {
    int time_left;
    size_t steam_released;
    std::string current;
    // either std::set<std::string> or BitMapType
    ValveTrackerType opened_valves;
    std::set<std::string>::iterator candidate_iter;
    size_t child_relative_max = 0;
};

template<class InputIt>
std::vector<std::set<typename InputIt::value_type>>
generate_combinations(InputIt first, InputIt end, int nr_of_objects, int sample_size) {
    // get value type from iterator; need to use typename to tell the following is a type
    std::vector<std::set<typename InputIt::value_type>> result;

    // src: https://stackoverflow.com/a/9430993 mitchnull
    // fill selector array that tells us wheter that index/item is active during
    // the current permutation
    std::vector<bool> v(nr_of_objects);
    std::fill(v.begin(), v.begin() + sample_size, true);

    std::vector<typename InputIt::value_type> vals;
    std::copy(first, end, std::inserter(vals, vals.end()));

    // use current order on first iter
    do {
        std::set<typename InputIt::value_type> s;
        // works for combinations since it results in sorted permutations
        for (int i = 0; i < nr_of_objects; ++i) {
            if (v[i]) {
                // take from input iterator
                s.insert(vals[i]);
            }
        }
        result.push_back(s);
       // v will swap around v to the prev permutation
       //   (returns false if there are no more permutations)
    } while (std::prev_permutation(v.begin(), v.end()));

    return result;
}

// tests whether two sets (or any other sorted data structure) share any elements
template<class SetA, class SetB, typename Compare = std::less<> >
bool share_elements(SetA&& a, SetB&& b, Compare comp = Compare()) {
    auto ita = a.begin();
    auto itb = b.begin();
    while (ita != a.end() && itb != b.end()) {
        if (comp(*ita, *itb)) {
            // ita is smaller than itb
            // (if using default std::less<>)
            ita++;
        } else if (comp(*itb, *ita)) {
            // itb smaller than ita
            itb++;
        } else {
            // equal
            return true;
        }
    }

    return false;
}

// ~26s for pt2 ~350MB... same algo in python also takes ~27s :O
// (unoptimized) ~130s for pt2
// NOTE: using bitmap for storing opened valves state (instead of set, so it's easily copyable and small)
// alternative: using a bitstring ('0' for closed, '1' for open)
size_t dfs_bitmap(std::map<std::string, Valve>& valves, const std::set<std::string>& open_candidates,
           BitMapType opened_valves,
           // time_left, current, open valves -> steam released RELATIVE from the time left, pos etc.
           // absolute does not make sense since you can reach the same tl,pos,opened with
           // different orders and thus different absolute steam_released values
           std::map<std::tuple<int, std::string, BitMapType>, size_t>& seen,
           int time_left, std::string current) {
    // already seen this state
    if (seen.count(std::make_tuple(time_left, current, opened_valves)) > 0) {
        return seen[std::make_tuple(time_left, current, opened_valves)];
    }

    size_t max = 0;

    auto& current_valve = valves[current];
    // try all the options of what valve to open next
    for (const auto& k : open_candidates) {
        const auto& valve = valves[k];
        BitMapType valve_bit = 1 << valve.valve_idx;
        // already opened
        if (opened_valves & valve_bit) continue;
        // calc time after traveling to valve with key k from current valve
        // - 1 to open the valve
        int time_remaining = time_left - current_valve.path_costs[k] - 1;
        // can use leq since if the time_remaining==0 it won't tick anymore
        if (time_remaining <= 0) continue;

        max = std::max(max,
                // recurse down into that branch
                // -> max of that branch
                // use binary or | to activate valve bit
                dfs_bitmap(valves, open_candidates, opened_valves | valve_bit, seen, time_remaining, k)
                    // plus the current valve we opened
                    + valve.flow_rate * time_remaining);
    }

    // memoize
    seen[std::make_tuple(time_left, current, opened_valves)] = max;

    return max;
}

// ~180s for pt2
// ~230s with memoization :O
size_t dfs(std::map<std::string, Valve>& valves, const std::set<std::string>& open_candidates,
           std::set<std::string> opened_valves,
           // time_left, current, open valves -> steam released RELATIVE from the time left, pos etc.
           // absolute does not make sense since you can reach the same tl,pos,opened with
           // different orders and thus different absolute steam_released values
           std::map<std::tuple<int, std::string, std::set<std::string>>, size_t>& seen,
           int time_left, std::string current) {
    // already seen this state
    if (seen.count(std::make_tuple(time_left, current, opened_valves)) > 0) {
        return seen[std::make_tuple(time_left, current, opened_valves)];
    }

    size_t max = 0;

    auto& current_valve = valves[current];
    // try all the options of what valve to open next
    for (const auto& k : open_candidates) {
        // already opened
        if (opened_valves.count(k) > 0) continue;
        const auto& valve = valves[k];
        // calc time after traveling to valve with key k from current valve
        // - 1 to open the valve
        int time_remaining = time_left - current_valve.path_costs[k] - 1;
        // can use leq since if the time_remaining==0 it won't tick anymore
        if (time_remaining <= 0) continue;

        auto with_k_opened = std::set<std::string>(opened_valves);
        with_k_opened.insert(k);

        max = std::max(max,
                // recurse down into that branch
                // -> max of that branch
                dfs(valves, open_candidates, with_k_opened, seen, time_remaining, k)
                    // plus the current valve we opened
                    + valve.flow_rate * time_remaining);
    }

    // memoize
    seen[std::make_tuple(time_left, current, opened_valves)] = max;

    return max;
}

// ~33s for pt2 ~350MB
size_t max_steam_bitmap(
        std::map<std::string, Valve>& valves, const std::set<std::string>& open_candidates,
        BitMapType starting_opened_valves,
        // time_left, current, open valves -> steam released RELATIVE from the time left, pos etc.
        // absolute does not make sense since you can reach the same tl,pos,opened with
        // different orders and thus different absolute steam_released values
        std::map<std::tuple<int, std::string, BitMapType>, size_t>& seen,
        const int start_time) {
    // not needed, we could also use the root nodes' child_relative_max
    size_t max_released = 0;
    // emulating a recursive DFS solution by keeping a stack of "nodes"
    // like in a recursive DFS one branch will be completely exhausted and then
    // that node will update it's parent's (over/left to it in the "stack"/vector)
    // child_relative_max values
    // we can use these to know what a max value of steam released can be achieved starting
    // from that state, which we will also memoize
    // (not using abs steam released, since you can reach the same state of time_left, current, opened_valves
    //  with different opening orders and thus different absolute steam_released values)
    // -> this is also how the reursive DFS method works
    std::vector<SearchStateMem<BitMapType>> states{
        { start_time, 0, "AA", starting_opened_valves, open_candidates.begin() } };
    while (states.size() > 0) {
        auto& state = states.back();

        if (state.candidate_iter == open_candidates.end()) {
            // memoize the relative maximum steam released that can be achieved from this state,
            // since we have explored all possible permutations from this node forwards
            seen[std::make_tuple(state.time_left, state.current, state.opened_valves)] =
                state.child_relative_max;

            // exclude start node from updating parent
            if (state.current == "AA") {
                states.pop_back();
                continue;
            }

            // update parent's child_relative_max
            // which is our child_relative_max + the difference to our parent's absolute steam_released
            auto& parent = states[states.size() - 2];
            auto this_relative_max =
                state.child_relative_max + (state.steam_released - parent.steam_released);
            if (this_relative_max > parent.child_relative_max) {
                parent.child_relative_max = this_relative_max;
            }

            auto absolute_max = state.steam_released + state.child_relative_max;
            if (absolute_max > max_released) max_released = absolute_max;

            states.pop_back();
            continue;
        }

        if (seen.count(std::make_tuple(state.time_left, state.current, state.opened_valves)) > 0) {
            // had the same state before
            auto relative_max =
                seen[std::make_tuple(state.time_left, state.current, state.opened_valves)];

            auto absolute_max = state.steam_released + relative_max;
            if (absolute_max > max_released) max_released = absolute_max;

            // exclude from updating parent
            if (state.current == "AA") {
                states.pop_back();
                continue;
            }

            // propagate new child_relative_max to parent (see above) @Repetition
            auto& parent = states[states.size() - 2];
            auto this_relative_max = relative_max + (state.steam_released - parent.steam_released);
            if (this_relative_max > parent.child_relative_max) {
                parent.child_relative_max = this_relative_max;
            }

            // remove queued node
            states.pop_back();
            continue;
        }

        auto& current_valve = valves[state.current];
        // queue one valve/node of all the possible ones then do the same for that node...
        // so we emulate a recursive dfs
        // (we store the iterator on state on our "emulated stack")
        for (; state.candidate_iter != open_candidates.end(); ++state.candidate_iter) {
            auto k = *state.candidate_iter;
            const auto& valve = valves[k];
            BitMapType valve_bit = (1 << valve.valve_idx);
            // already opened
            if (state.opened_valves & valve_bit) continue;
            // calc time after traveling to valve with key k from current valve
            // - 1 to open the valve
            int time_remaining = state.time_left - current_valve.path_costs[k] - 1;
            // can use leq since if the time_remaining==0 it won't tick anymore
            if (time_remaining <= 0) continue;

            states.emplace_back(
                SearchStateMem<BitMapType>{
                  time_remaining,
                  // steam released at the end, including new one
                  state.steam_released + time_remaining * valve.flow_rate,
                  k, // current
                  // add activated valve by binary ORing
                  state.opened_valves | valve_bit,
                  open_candidates.begin()
                }
            );

            // start on next candidate
            state.candidate_iter++;
            // only queue one node so we can figure out the max of that one specific branch
            // before queueing alternatives
            break;
        }
    }

    return max_released;
}

// ~270s for pt2
size_t max_steam(std::map<std::string, Valve>& valves, const std::set<std::string>& open_candidates,
                 const std::set<std::string>& starting_opened_valves,
                 // time_left, current, open valves -> steam released RELATIVE from the time left, pos etc.
                 // absolute does not make sense since you can reach the same tl,pos,opened with
                 // different orders and thus different absolute steam_released values
                 std::map<std::tuple<int, std::string, std::set<std::string>>, size_t>& seen,
                 const int start_time) {
    // not needed, we could also use the root nodes' child_relative_max
    size_t max_released = 0;
    // emulating a recursive DFS solution by keeping a stack of "nodes"
    // like in a recursive DFS one branch will be completely exhausted and then
    // that node will update it's parent's (over/left to it in the "stack"/vector)
    // child_relative_max values
    // we can use these to know what a max value of steam released can be achieved starting
    // from that state, which we will also memoize
    // (not using abs steam released, since you can reach the same state of time_left, current, opened_valves
    //  with different opening orders and thus different absolute steam_released values)
    // -> this is also how the reursive DFS method works
    std::vector<SearchStateMem<std::set<std::string>>> states{
        { start_time, 0, "AA", starting_opened_valves, open_candidates.begin() } };
    while (states.size() > 0) {
        auto& state = states.back();

        if (state.candidate_iter == open_candidates.end()) {
            // memoize the relative maximum steam released that can be achieved from this state,
            // since we have explored all possible permutations from this node forwards
            seen[std::make_tuple(state.time_left, state.current, state.opened_valves)] =
                state.child_relative_max;

            // exclude start node from updating parent
            if (state.current == "AA") {
                states.pop_back();
                continue;
            }

            // update parent's child_relative_max
            // which is our child_relative_max + the difference to our parent's absolute steam_released
            auto& parent = states[states.size() - 2];
            auto this_relative_max =
                state.child_relative_max + (state.steam_released - parent.steam_released);
            if (this_relative_max > parent.child_relative_max) {
                parent.child_relative_max = this_relative_max;
            }

            auto absolute_max = state.steam_released + state.child_relative_max;
            if (absolute_max > max_released) max_released = absolute_max;

            states.pop_back();
            continue;
        }

        if (seen.count(std::make_tuple(state.time_left, state.current, state.opened_valves)) > 0) {
            // had the same state before
            auto relative_max =
                seen[std::make_tuple(state.time_left, state.current, state.opened_valves)];

            auto absolute_max = state.steam_released + relative_max;
            if (absolute_max > max_released) max_released = absolute_max;

            // exclude from updating parent
            if (state.current == "AA") {
                states.pop_back();
                continue;
            }

            // propagate new child_relative_max to parent (see above) @Repetition
            auto& parent = states[states.size() - 2];
            auto this_relative_max = relative_max + (state.steam_released - parent.steam_released);
            if (this_relative_max > parent.child_relative_max) {
                parent.child_relative_max = this_relative_max;
            }

            // remove queued node
            states.pop_back();
            continue;
        }

        auto& current_valve = valves[state.current];
        // queue one valve/node of all the possible ones then do the same for that node...
        // so we emulate a recursive dfs
        // (we store the iterator on state on our "emulated stack")
        for (; state.candidate_iter != open_candidates.end(); ++state.candidate_iter) {
            auto k = *state.candidate_iter;
            // already opened
            if (state.opened_valves.count(k) > 0) continue;
            const auto& valve = valves[k];
            // calc time after traveling to valve with key k from current valve
            // - 1 to open the valve
            int time_remaining = state.time_left - current_valve.path_costs[k] - 1;
            // can use leq since if the time_remaining==0 it won't tick anymore
            if (time_remaining <= 0) continue;

            auto with_k_opened = std::set<std::string>(state.opened_valves);
            with_k_opened.insert(k);

            states.emplace_back(
                SearchStateMem<std::set<std::string>>{
                  time_remaining,
                  // steam released at the end, including new one
                  state.steam_released + time_remaining * valve.flow_rate,
                  k, // current
                  with_k_opened,
                  open_candidates.begin()
                }
            );

            // start on next candidate
            state.candidate_iter++;
            // only queue one node so we can figure out the max of that one specific branch
            // before queueing alternatives
            break;
        }
    }

    return max_released;
}

int main() {
    std::ifstream infile("d16.in");
    std::string line;
    std::map<std::string, Valve> valves;
    while (getline(infile, line)) {
        Valve v;

        int valve_key_start = 6;
        int valve_key_end = line.find(" ", valve_key_start);
        v.key = line.substr(valve_key_start, valve_key_end - valve_key_start);

        int flow_rate_start = valve_key_end + 15;
        int flow_rate_end = line.find(";", flow_rate_start);
        std::from_chars(&line[flow_rate_start], &line[flow_rate_end], v.flow_rate);

        // using proper grammer... tunnel/s lead/s...
        // so can't use static offset
        SplitIter tunnels_iter(line, " ", flow_rate_end + 2);
        tunnels_iter.skip(); // tunnel/s
        tunnels_iter.skip(); // lead/s
        tunnels_iter.skip(); // to
        tunnels_iter.skip(); // valve/s
        tunnels_iter.change_pattern(", ");
        std::string tunnel_key;
        while (tunnels_iter.more()) {
            tunnels_iter.next(tunnel_key);
            v.tunnels.push_back(tunnel_key);
        }

#if 0
        std::cout << "Valve: " << v.key << "i " << v.valve_idx << " rate " << v.flow_rate << " tunnels:" << std::endl;
        for (const auto& s : v.tunnels) {
            std::cout << s << " ";
        }
        std::cout << std::endl;
#endif

        valves[v.key] = v;
    }

    // save non-zero flow rate valves so we can reduce the search space
    using OpenedValves = std::set<std::string>;
    OpenedValves open_candidates;
    int valve_idx = 0;
    // map __shortest__ distances between nodes, so we can easily look them up
    // cant use const ref below since [] can modify the map
    for (auto& [key, valve] : valves) {
        if (valve.flow_rate > 0) {
            open_candidates.insert(valve.key);
            valve.valve_idx = valve_idx;
            valve_idx += 1;
        }

        // BFS traversal of connected nodes
        // (for an undirected, unweighted graph BFS gives us the shortest path cost)
        // ok to take a pointer to sth in the map here since we don't insert afterwards anymore
        std::deque<Path> q{ { 0, key } };
        std::set<std::string> seen;
        while (!q.empty()) {
            auto current_path = q.front();
            q.pop_front();
            seen.insert(current_path.key);

            // first (and only time) a node is visted, update path costs
            valve.path_costs[current_path.key] = current_path.path_cost;

            auto& current_node = valves[current_path.key];
            // q conneced nodes
            for (const auto& vkey : current_node.tunnels) {
                if (seen.count(vkey) > 0) continue;
                q.push_back({ current_path.path_cost + 1, vkey });
            }
        }

        // std::cout << "shortest path (" << key << "):" << std::endl;
        // for (const auto& [k,v] : valve.path_costs) {
        //     std::cout << k << " -> " << v << std::endl;
        // }
    }


    auto start_pt1 = std::chrono::high_resolution_clock::now();
    
#if USE_SET
#if RECURSIVE
    {
        std::map<std::tuple<int, std::string, std::set<std::string>>, size_t> seen;
        size_t max_released = dfs(valves, open_candidates, std::set<std::string>(), seen, 30, "AA");
        std::cout << "Part1 (set+rec): Maximum steam release: " << max_released << std::endl;
        max_released = dfs(valves, open_candidates, std::set<std::string>(), seen, 30, "AA");
        std::cout << "Part1 (set+rec): Maximum steam release: " << max_released << std::endl;
    }
#else
    {
        std::map<std::tuple<int, std::string, std::set<std::string>>, size_t> seen;
        size_t max_released = max_steam(valves, open_candidates, std::set<std::string>(), seen, 30);
        std::cout << "Part1 (set+iter): Maximum steam release: " << max_released << std::endl;
        max_released = max_steam(valves, open_candidates, std::set<std::string>(), seen, 30);
        std::cout << "Part1 (set+iter): Maximum steam release: " << max_released << std::endl;
    }
#endif
#else
#if RECURSIVE
    {
        std::map<std::tuple<int, std::string, BitMapType>, size_t> seen;
        size_t max_released = dfs_bitmap(valves, open_candidates, 0, seen, 30, "AA");
        std::cout << "Part1 (bitmap+rec): Maximum steam release: " << max_released << std::endl;
        max_released = dfs_bitmap(valves, open_candidates, 0, seen, 30, "AA");
        std::cout << "Part1 (bitmap+rec): Maximum steam release: " << max_released << std::endl;
    }
#else
    {
        std::map<std::tuple<int, std::string, BitMapType>, size_t> seen;
        size_t max_released = max_steam_bitmap(valves, open_candidates, 0, seen, 30);
        std::cout << "Part1 (bitmap+iter): Maximum steam release: " << max_released << std::endl;
        max_released = max_steam_bitmap(valves, open_candidates, 0, seen, 30);
        std::cout << "Part1 (bitmap+iter): Maximum steam release: " << max_released << std::endl;
    }
#endif
#endif

    auto start_pt2 = std::chrono::high_resolution_clock::now();
    auto duration_pt1 = std::chrono::duration_cast<std::chrono::milliseconds>(start_pt2 - start_pt1);
    std::cout << "Part1 took " << duration_pt1.count() << "ms" << std::endl;

#if USE_SET // using sets and generating combinations
    size_t max_combined = 0;
    int nr_of_objects = open_candidates.size();
    std::cout << "Non-zero valves " << nr_of_objects << std::endl;
    // time_left, current, open valves -> steam released
    std::map<std::tuple<int, std::string, std::set<std::string>>, size_t> seen;
    // try every combination of valves that are split between me or the elephant,
    // whereby me and the elephant are interchangeable workers though,
    // thus we only need to go up to half of the combinations
    for (int sample_size = 1; sample_size <= (open_candidates.size() / 2); ++sample_size) {
        auto combinations = generate_combinations(
                open_candidates.begin(), open_candidates.end(), nr_of_objects, sample_size);
        for (const auto& available_valves : combinations) {
            // for (const auto& s : available_valves) {
            //     std::cout << s << " ";
            // }
            // std::cout << std::endl;
            
            // build difference with all valves -> other worker's valves
            OpenedValves diff;
            std::set_difference(open_candidates.begin(), open_candidates.end(),
                                available_valves.begin(), available_valves.end(),
                                std::inserter(diff, diff.begin()));
            // std::cout << "Diff:";
            // for (const auto& s : diff) {
            //     std::cout << s << " ";
            // }
            // std::cout << std::endl;

#if RECURSIVE
            auto maxme = dfs(valves, open_candidates, available_valves, seen, 26, "AA");
            auto maxele = dfs(valves, open_candidates, diff, seen, 26, "AA");
#else
            auto maxme = max_steam(valves, open_candidates, available_valves, seen, 26);
            auto maxele = max_steam(valves, open_candidates, diff, seen, 26);
#endif
            if ((maxme + maxele) > max_combined) {
                max_combined = maxme + maxele;
            }
        }
    }

#if RECURSIVE
    std::cout << "Part2 (set+combinations+rec): Maximum steam released: " << max_combined << std::endl;
#else
    std::cout << "Part2 (set+combinations+iter): Maximum steam released: " << max_combined << std::endl;
#endif

#else

    size_t max_combined = 0;
    int nr_of_objects = open_candidates.size();
    std::cout << "Non-zero valves " << nr_of_objects << std::endl;
    // time_left, current, open valves -> steam released
    std::map<std::tuple<int, std::string, BitMapType>, size_t> seen;
    // try every combination of valves that are split between me or the elephant,
    // whereby me and the elephant are interchangeable workers though,
    // thus we only need to go up to half of the combinations
    //
    // since we use a bitmap to represent opened/closed valves (1/0-bit) we can count up to the 
    // value where all the bits of expected valves are active
    // -> use pow of 2 (left shifting) then -1 to activate all the bits
    BitMapType all_valve_states = (1 << nr_of_objects) - 1;
    for (BitMapType available_valves = 0; available_valves <= (all_valve_states / 2); ++available_valves) {
        // all valves XORed with the human valves will give us the valves of the elephant
        BitMapType diff = all_valve_states ^ available_valves;
#if RECURSIVE
        auto maxme = dfs_bitmap(valves, open_candidates, available_valves, seen, 26, "AA");
        auto maxele = dfs_bitmap(valves, open_candidates, diff, seen, 26, "AA");
#else
        auto maxme = max_steam_bitmap(valves, open_candidates, available_valves, seen, 26);
        auto maxele = max_steam_bitmap(valves, open_candidates, diff, seen, 26);
#endif
        if ((maxme + maxele) > max_combined) {
            max_combined = maxme + maxele;
        }
    }

#if RECURSIVE
    std::cout << "Part2 (bitmap+rec): Maximum steam released: " << max_combined << std::endl;
#else
    std::cout << "Part2 (bitmap+iterative): Maximum steam released: " << max_combined << std::endl;
#endif
#endif


    // dfs_bitmap
    // ~26s for pt2 ~350MB... same algo in python: also takes ~27s :O ~550MB
    // (unoptimized) ~130s for pt2
    // dfs
    // ~180s for pt2
    // ~230s with memoization :O
    // iterative bitmap
    // ~33s for pt2 ~350MB
    // iterative sets
    // ~270s for pt2
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start_pt2);
    std::cout << "Part2 took " << duration.count() << "ms" << std::endl;
}
