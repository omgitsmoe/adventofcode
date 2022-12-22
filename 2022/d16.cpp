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

template<typename T>
std::vector<T> generate_combinations(const std::vector<T>& in, int nr_of_objects, int sample_size) {
    std::vector<T> result;

    // src: https://stackoverflow.com/a/9430993 mitchnull
    // fill selector array that tells us wheter that index/item is active during
    // the current permutation
    std::vector<bool> v(nr_of_objects);
    std::fill(v.begin(), v.begin() + sample_size, true);

    // use current order on first iter
    do {
        // works for combinations since it results in sorted permutations
        for (int i = 0; i < nr_of_objects; ++i) {
            if (v[i]) {
                result.push_back(in[i]);
            }
        }
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

#if 0 // does not work, since it just picks the move that gives the best pressure per step
      // without considering moves after it
std::set<std::string> max_steam(
    const std::vector<std::string>& open_candidates,
    std::map<std::string, Valve>& valves,
    const int start_time_left,
    std::set<std::string> start_opened_valves,
    size_t& out_max_released,
    bool part2
) {
    std::set<std::string> max_opened;
    size_t max_released = 0;
    std::vector<SearchState> states{ { start_time_left, 0, 0, "AA", start_opened_valves } };
    while (states.size() > 0) {
        auto state = states.back();
        states.pop_back();

        auto& current_valve = valves[state.current];
        bool is_start = state.time_left == start_time_left;
        bool moved = false;
        std::string best;
        float max_steam_per_step = 0.f;
        // queue all possible nodes that we can move to next
        for (const auto& k : open_candidates) {
            // already opened
            if (state.opened_valves.count(k) > 0) continue;
            // minutes to travel to valve with key k from current valve
            // + 1 to open the valve
            int cost = current_valve.path_costs[k] + 1;
            if (cost > state.time_left) continue;

            const auto& valve = valves[k];
            // figure out how much steam release the candidate valve gives us if we choose
            // to activate it next
            float steam_per_step = static_cast<float>((state.time_left - cost) * valve.flow_rate) /
                                   static_cast<float>(cost);
            if (steam_per_step > max_steam_per_step) {
                best = k;
                max_steam_per_step = steam_per_step;
                moved = true;
            }
            // std::cout << state.current << " -> " << k << ": " << cost << std::endl;


            // TODO remove?
#if 0
            if (is_start && steam_per_step > 0) {
                // queue all nodes to start with
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
#endif
        }


        // only open the best valve, which gives us the most amount of released pressure
        // for steps invested
        // (was previously just queuing all nodes above)
        if (/*!is_start && */max_steam_per_step > 0) {
            auto with_k_opened = std::set<std::string>(state.opened_valves);
            with_k_opened.insert(best);
            const auto& valve = valves[best];
            int cost = current_valve.path_costs[best] + 1;
            std::cout << best << std::endl;

            states.emplace_back(
                SearchState{
                  state.time_left - cost, // time left after opening
                  state.steam_released + state.flow_per_minute * cost, // steam released (not including new one)
                  state.flow_per_minute + valve.flow_rate, // flow_per_minute
                  best, // current
                  with_k_opened
                }
            );
        }

#if 0
        if (part2) {
            // after the human opens any valve
            // find the optimal path for the elephant with the current path of the human/set of
            // opened valves
            // (needs to be done after every move, so we account for solutions where the human
            // can open all/most of the valves but could open them sooner with the help of the
            // elephant)
            //
            // calculate total if there's time left
            size_t released_total = state.time_left > 0 
                ? state.steam_released + state.flow_per_minute * state.time_left
                : state.steam_released;

            // optimal path for the elephant with the current opened_valves
            size_t with_ele_max;
            max_steam(open_candidates, valves, start_time_left, state.opened_valves, with_ele_max, false);
            std::cout << "Me " << released_total << " with ele " << with_ele_max << std::endl;
            released_total += with_ele_max;

            if (released_total > max_released) {
                // std::cout << "New max: " << state.current << " tl " << state.time_left << " fpm " << state.flow_per_minute << " released " << state.steam_released << " total " << released_total << std::endl;
                max_released = released_total;
                max_opened = state.opened_valves;
            }
        }
#endif

        if (!moved) {
            // calculate total if there's time left
            size_t released_total = state.time_left > 0 
                ? state.steam_released + state.flow_per_minute * state.time_left
                : state.steam_released;

            // find the optimal path for the elephant after the human has run out of time
            // NOTE: this will not work, since the human might've already opened all the
            // valves, but together with the elephant they could've been opened sooner,
            // which can't be checked here, but needs to be done after __every time__
            // the has opened a valve (see above)
#if 0
            if (part2) {
                size_t with_ele_max;
                max_steam(open_candidates, valves, start_time_left, state.opened_valves, with_ele_max, false);
                // std::cout << "Me " << released_total << " with ele " << with_ele_max << std::endl;
                released_total += with_ele_max;
            }
#endif

            if (released_total > max_released) {
                // std::cout << "New max: " << state.current << " tl " << state.time_left << " fpm " << state.flow_per_minute << " released " << state.steam_released << " total " << released_total << std::endl;
                max_released = released_total;
                max_opened = state.opened_valves;
            }
        }
    }

    out_max_released = max_released;

    return max_opened;
}

#endif

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
    std::vector<std::string> open_candidates;
    // map __shortest__ distances between nodes, so we can easily look them up
    // cant use const ref below since [] can modify the map
    for (auto& [key, valve] : valves) {
        if (valve.flow_rate > 0)
            open_candidates.push_back(valve.key);

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
            // q connected nodes
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

    
    // NOTE: testing all candidates instead of only the best one
    {
        size_t max_released = 0;
        std::vector<SearchState> states{ { 30, 0, 0, "AA", std::set<std::string>() } };
        while (states.size() > 0) {
            auto state = states.back();
            states.pop_back();

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
        }

        std::cout << "Part1: Maximum steam release: " << max_released << std::endl;
    }

#if 0
    {
        size_t max_released;
        max_steam(open_candidates, valves, 30, std::set<std::string>(), max_released, false);
        std::cout << "Part1: Maximum steam release: " << max_released << std::endl;
    }
#endif

    auto start_pt2 = std::chrono::high_resolution_clock::now();

#if USE_COMBINATIONS
    using OpenedValves = std::set<std::string>;
    std::vector<OpenedValves> allowed_valves;
    int nr_of_objects = open_candidates.size();
    int sample_size = (open_candidates.size() + 0.5f) / 2;
    auto combinations = generate_combinations(open_candidates, nr_of_objects, sample_size);
    OpenedValves s;
    for (int i = 0; i < combinations.size(); ++i) {
        s.insert(combinations[i]);
        if ((i + 1) % sample_size == 0) {
            allowed_valves.push_back(s);
            s.clear();
        }
    }

    std::vector<std::tuple<OpenedValves, OpenedValves>> valve_splits;
    for (const auto& a : allowed_valves) {
        for (const auto& b : allowed_valves) {
            if (!share_elements(a, b)) {
                valve_splits.emplace_back(std::make_tuple(a, b));
                valve_splits.emplace_back(std::make_tuple(b, a));
            }
        }
    }
#endif

#if 0
    // trying to generate combinations...
    std::vector<int> stack(sample_size, 0);
    // we can start at 0, 1, .. sample_size-1
    for (int i = 0; i < sample_size; ++i) stack[i] = i;
    int level = sample_size - 1;
    int reset = 0;
    while (true) {
        int& level_idx = stack[level];
        bool reached_end_idx = level_idx >= nr_of_objects;
        if (reached_end_idx) {
            if (level == (sample_size - 1)) {
                if (reset == (nr_of_objects - 1)) {
                    // end
                    break;
                }
                // increase value we reset a "digit"/idx to
                level_idx = reset;
                reset += 1;
            }
            level_idx = reset;
            level = (level + 1) % sample_size;
            continue;
        }

        // this would just produce the permutations
        // only output if index are rev sorted (since we start increasing idx at the front)
        bool rev_sorted = true;
        // assuming at least sample size 2
        for (int i = 1; i < sample_size; ++i) {
            // asc order -> break
            if (stack[i] > stack[i-1]) {
                rev_sorted = false;
                break;
            }
        }

        if (rev_sorted) {
            std::set<std::string> allowed;
            for (int i = 0; i < sample_size; ++i) {
                int idx = stack[i];
                // hard coded src
                allowed.insert(open_candidates[idx]);
            }
            allowed_valves.push_back(allowed);
        }

        std::cout << "l" << level << std::endl;
        for (auto i : stack) {
            std::cout << i << " ";
        }
        std::cout << std::endl;
        level_idx += 1;
    }
#endif
#if PRINT_COMBINATION_SPLITS
    std::cout << "comb done" << std::endl;
    for (auto const& [a, b] : valve_splits) {
        std::cout << "Pair:" << std::endl;
        for (auto const& s : {a, b}) {
            for (auto const& ss : s) {
                std::cout << ss;
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }
#endif

#if USE_COMBINATIONS
    size_t max_combined = 0;
    for (auto const& [a, b] : valve_splits) {
        size_t max_me;
        size_t max_el;
        max_steam(open_candidates, valves, 26, a, max_me, false);
        max_steam(open_candidates, valves, 26, b, max_el, false);
        if ((max_me + max_el) > max_combined) {
            max_combined = max_me + max_el;
        }
    }
    std::cout << "Part2 (combs): Maximum steam release: " << max_combined << std::endl;
#endif

#if !defined(USE_COMBINATIONS)
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
