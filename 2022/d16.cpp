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

struct Valve {
    std::string key;
    int flow_rate;
    bool open = false;
    std::vector<std::string> tunnels;
    std::map<std::string, int> path_costs;
};

struct Path {
    int path_cost;
    std::string key;
};

struct SearchState {
    int time_left;
    size_t steam_released;
    int flow_per_minute;
    std::string current;
    std::set<std::string> opened_valves;
};
struct SearchStateMem {
    int time_left;
    size_t steam_released;
    std::string current;
    std::set<std::string> opened_valves;
    // std::vector<std::tuple<int, std::string, std::set<std::string>>> prevs;
    // std::tuple<int, std::string, std::set<std::string>> prev;
    std::set<std::string>::iterator candidate_iter;
    size_t child_relative_max = 0;
};

struct SearchStateCore {
    int time_left;
    size_t steam_released;
    int flow_per_minute;
    std::string current;
};
struct SearchStatePt2 {
    SearchStateCore me;
    SearchStateCore elephant;
    std::set<std::string> opened_valves;
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
    std::vector<SearchStateMem> states{ { start_time, 0, "AA", starting_opened_valves,
                                          open_candidates.begin() } };
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
                SearchStateMem{
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
        std::cout << "Valve: " << v.key << " rate " << v.flow_rate << " tunnels:" << std::endl;
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
    // map __shortest__ distances between nodes, so we can easily look them up
    // cant use const ref below since [] can modify the map
    for (auto& [key, valve] : valves) {
        if (valve.flow_rate > 0)
            open_candidates.insert(valve.key);

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

    
#if 0
    // NOTE: testing all candidates instead of only the best one, which I assume is necessary
    // because we could otherwise miss optimal states
    // -> use memoization instead to reduce the computation needed
    // -> not really memoization since we don't save steam_released and just skip
    //    since we must've already queued candidates
    {
        size_t max_released = 0;
        std::vector<SearchState> states{ { 30, 0, 0, "AA", std::set<std::string>() } };
        // memoize previously computed states, so we don't have to queue them up with all their
        // options of what valve to visit next -> Dynamic Programming using Memoization
        // time left, steam released, current pos, opened valves
        // NOTE: compared to a recursive approach this is not reusable
        std::set<std::tuple<int, size_t, std::string, OpenedValves>> seen;
        while (states.size() > 0) {
            auto state = states.back();
            states.pop_back();

            if (seen.count(std::make_tuple(
                            state.time_left, state.steam_released, state.current, state.opened_valves)) > 0) {
                // had the same state before
                continue;
            }

            auto& current_valve = valves[state.current];
            bool moved = false;
            // queue all possible nodes that we can move to next
            for (const auto& k : open_candidates) {
                // already opened
                if (state.opened_valves.count(k) > 0) continue;
                const auto& valve = valves[k];
                // minutes to travel to valve with key k from current valve
                // + 1 to open the valve
                int cost = current_valve.path_costs[k] + 1;
                // std::cout << state.current << " -> " << k << ": " << cost << std::endl;
                if (cost > state.time_left) continue;
                moved = true;

                auto with_k_opened = std::set<std::string>(state.opened_valves);
                with_k_opened.insert(k);

                states.emplace_back(
                    SearchState{
                      state.time_left - cost, // time left after opening
                      state.steam_released + state.flow_per_minute * cost, // steam released (not including new one)
                      state.flow_per_minute + valve.flow_rate, // flow_per_minute
                      k, // current
                      with_k_opened
                    }
                );
            }

            if (!moved) {
                // calculate total if there's time left
                size_t released_total = state.time_left > 0 
                    ? state.steam_released + state.flow_per_minute * state.time_left
                    : state.steam_released;
                if (released_total > max_released) {
                    // std::cout << "New max: " << state.current << " tl " << state.time_left << " fpm " << state.flow_per_minute << " released " << state.steam_released << " total " << released_total << std::endl;
                    max_released = released_total;
                }
            }

            // mark this state as seen
            seen.insert(
                std::make_tuple(state.time_left, state.steam_released, state.current, state.opened_valves));
        }

        std::cout << "Part1: Maximum steam release: " << max_released << std::endl;
    }
#endif

#if 1
    {
        std::map<std::tuple<int, std::string, std::set<std::string>>, size_t> seen;
        size_t max_released = max_steam(valves, open_candidates, std::set<std::string>(), seen, 30);
        std::cout << "Part1: Maximum steam release: " << max_released << std::endl;
        max_released = max_steam(valves, open_candidates, std::set<std::string>(), seen, 30);
        std::cout << "Part1: Maximum steam release: " << max_released << std::endl;
    }
#endif

    auto start_pt2 = std::chrono::high_resolution_clock::now();

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

            auto maxme = max_steam(valves, open_candidates, available_valves, seen, 26);
            auto maxele = max_steam(valves, open_candidates, diff, seen, 26);
            if ((maxme + maxele) > max_combined) {
                max_combined = maxme + maxele;
            }
        }
    }

    std::cout << "Part2 (all combs): Maximum steam released: " << max_combined << std::endl;


#if 0 //!defined(USE_COMBINATIONS)
    // pre-compute permutations
    std::set<std::tuple<std::string, std::string>> permutations;
    for (const auto& me : open_candidates) {
        for (const auto& el : open_candidates) {
            if (me == el) continue; // can't open same valve
            permutations.insert(std::make_tuple(me, el));
        }
    }
    // for (const auto& [kme, kel] : permutations) {
    //     std::cout << kme << "-" << kel << std::endl;
    // }

    // NOTE: solves correctly for the example and my input, but since we only look ahead one step
    // it is very likely wrong for other inputs
    // e.g. d16_other.in -> should be 2189; below gets: 2027
    size_t max_released = 0;
    std::vector<SearchStatePt2> states{ { { 26, 0, 0, "AA" }, { 26, 0, 0, "AA" }, std::set<std::string>() } };
    while (states.size() > 0) {
        auto state = states.back();
        states.pop_back();

        auto& current_valve_me = valves[state.me.current];
        auto& current_valve_ele = valves[state.elephant.current];
        bool moved = false;
        float best_steam_per_step_me = 0;
        float best_steam_per_step_ele = 0;
        std::string best_valve_me;
        std::string best_valve_ele;
        // find the best valve to open next for me and the elephant by calculating
        // how much amount of steam released per step we get
        for (const auto& k : open_candidates) {
            bool opened = state.opened_valves.count(k) > 0;
            if (opened) continue;
            // minutes to travel to valve with key k from current valve
            // + 1 to open the valve
            int cost_me = current_valve_me.path_costs[k] + 1;
            // NOTE: !IMPORTANT! me/elephant might have different costs due to different current positions
            int cost_ele = current_valve_ele.path_costs[k] + 1;

            const auto& valve = valves[k];
            if (state.me.time_left >= cost_me) {
                // steam released until end if we choose to open the valve next
                // / cost -> to get it per step
                float steam_per_step_me = 
                    static_cast<float>((state.me.time_left - cost_me) * valve.flow_rate) /
                    static_cast<float>(cost_me);
                if (steam_per_step_me > best_steam_per_step_me) {
                    best_steam_per_step_me = steam_per_step_me;
                    best_valve_me = k;
                    moved = true;
                }
            }

            if (state.elephant.time_left >= cost_ele) {
                float steam_per_step_ele = 
                    static_cast<float>((state.elephant.time_left - cost_ele) * valve.flow_rate) /
                    static_cast<float>(cost_ele);
                if (steam_per_step_ele > best_steam_per_step_ele) {
                    best_steam_per_step_ele = steam_per_step_ele;
                    best_valve_ele = k;
                    moved = true;
                }
            }
        }
        
        // example best paths:
        // ME  JJ-BB-CC
        // ELE DD-HH-EE
        // NOTE: doing the best steps in turns does not work:
        // Ele JJ
        // Me  DD-> next best step is going to BB
        // but I would be closer to HH, so Ele should open BB and I should go to HH
        // => add both the best step for me as well as the elephant to the queue
        if (moved) {
            for (auto [next, is_me, best_steam_per_step] :
                    { std::make_tuple(state.me, true, best_steam_per_step_me),
                      std::make_tuple(state.elephant, false, best_steam_per_step_ele) }) {
                // check if we actually have a step to make
                if (best_steam_per_step == 0) continue;
                std::string valve_key = is_me ? best_valve_me : best_valve_ele;
                auto& current_valve = valves[next.current];
                int cost = current_valve.path_costs[valve_key] + 1;
                const auto& valve = valves[valve_key];
                auto with_opened = std::set<std::string>(state.opened_valves);
                with_opened.insert(valve.key);

                next.time_left = next.time_left - cost; // time left after opening
                // steam released (not including new one)
                // -> ORDER important
                next.steam_released = next.steam_released + next.flow_per_minute * cost;
                next.flow_per_minute = next.flow_per_minute + valve.flow_rate; // flow_per_minute
                next.current = valve_key; // current
                if (is_me) {
                    // use old elephant
                    states.emplace_back(SearchStatePt2 { next, state.elephant, with_opened });
                } else {
                    // use old me state
                    states.emplace_back(SearchStatePt2 { state.me, next, with_opened });
                }
                // std::cout << "Me " << is_me << " v " << valve_key << " +flowr " << valve.flow_rate << " fpm " << next.flow_per_minute << std::endl;
            }
        } else {
            // calculate total if there's time left
            size_t released_total_me = state.me.time_left > 0 
                ? state.me.steam_released + state.me.flow_per_minute * state.me.time_left
                : state.me.steam_released;
            size_t released_total_ele = state.elephant.time_left > 0 
                ? state.elephant.steam_released + state.elephant.flow_per_minute * state.elephant.time_left
                : state.elephant.steam_released;
            size_t released_total = released_total_me + released_total_ele;
            if (released_total > max_released) {
                // std::cout << "New max: " << state.current << " tl " << state.time_left << " fpm " << state.flow_per_minute << " released " << state.steam_released << " total " << released_total << std::endl;
                // std::cout << "New max: " << released_total << std::endl;
                max_released = released_total;
            }
        }
    }

    std::cout << "Part2: Maximum steam released with the help of an elephant: " << max_released << std::endl;
#endif

    // pt2 for the example:
    // loop using SearchStatePt2 takes ~480us
    // USE_COMBINATIONS takes ~1000us
    // pt2 for the actual input:
    // loop SearchStatePt2 ~12ms
    // USE_COMBINATIONS ~8-9s
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start_pt2);
    std::cout << "Part2 took " << duration.count() << "ms" << std::endl;
}
