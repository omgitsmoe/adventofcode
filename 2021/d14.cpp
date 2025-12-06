#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <chrono>
#include <limits>

using ll = long long;

// NOTE: converted from Python using ChatGPT and then checked manually
// compile using:
// g++ -std=c++17 -O3 -march=native -o d14_cpp d14.cpp
// clang++ -std=c++17 -O3 -march=native -o aoc14 d14.cpp

std::unordered_map<std::string, std::pair<std::string, std::string>>
read_rules(const std::vector<std::string>& lines) 
{
    std::unordered_map<std::string, std::pair<std::string, std::string>> rules;

    for (size_t i = 2; i < lines.size(); i++) {
        if (lines[i].empty()) continue;

        std::size_t pos = lines[i].find(" -> ");
        std::string src = lines[i].substr(0, pos);
        std::string ins = lines[i].substr(pos + 4);

        std::string p1 = std::string() + src[0] + ins;
        std::string p2 = std::string() + ins + src[1];

        rules[src] = std::make_pair(p1, p2);
    }

    return rules;
}

std::unordered_map<std::string, ll>
apply_rules(
    const std::string& polymer,
    const std::unordered_map<std::string, std::pair<std::string, std::string>>& rules,
    int times
) {
    std::unordered_map<std::string, ll> pair_count;

    // initial pairs
    for (size_t i = 0; i + 1 < polymer.size(); i++) {
        std::string p = polymer.substr(i, 2);
        pair_count[p]++;
    }

    for (int step = 0; step < times; step++) {
        std::unordered_map<std::string, ll> new_pair_count = pair_count;

        for (const auto& kv : rules) {
            const std::string& src = kv.first;
            const auto& pair = kv.second;

            auto it = pair_count.find(src);
            if (it == pair_count.end() || it->second == 0)
                continue;

            ll count = it->second;

            new_pair_count[src] -= count;
            new_pair_count[pair.first] += count;
            new_pair_count[pair.second] += count;
        }

        pair_count = std::move(new_pair_count);
    }

    return pair_count;
}

std::unordered_map<char, ll>
count_chars(const std::unordered_map<std::string, ll>& pairs) 
{
    std::unordered_map<char, ll> char_count;

    for (const auto& kv : pairs) {
        const std::string& p = kv.first;
        ll count = kv.second;

        char_count[p[0]] += count;
        char_count[p[1]] += count;
    }

    return char_count;
}

std::unordered_map<char, ll>
correct_counts(const std::unordered_map<char, ll>& char_count)
{
    std::unordered_map<char, ll> corrected;
    for (const auto& kv : char_count) {
        corrected[kv.first] = static_cast<ll>(kv.second / 2.0 + 0.5);
    }
    return corrected;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    // read input
    std::ifstream f("d14.in");
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(f, line)) {
        lines.push_back(line);
    }

    std::string polymer = lines[0];
    auto rules = read_rules(lines);

    auto start = std::chrono::high_resolution_clock::now();

    auto pairs = apply_rules(polymer, rules, 40);
    auto counts = correct_counts(count_chars(pairs));

    ll mn = std::numeric_limits<ll>::max();
    ll mx = std::numeric_limits<ll>::min();

    for (const auto& kv : counts) {
        mn = std::min(mn, kv.second);
        mx = std::max(mx, kv.second);
    }

    auto end = std::chrono::high_resolution_clock::now();
    double ms = std::chrono::duration<double, std::milli>(end - start).count();

    std::cout << "Part2: " << (mx - mn) << "\n";
    std::cout << "Took " << ms << " ms\n";
}
