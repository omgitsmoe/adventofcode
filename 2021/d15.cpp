#include <cassert>
#include <iostream>
#include <chrono>
#include <fstream>
#include <array>
#include <unordered_set>
#include <vector>
#include <set>
#include <string>
#include <limits>
#include <tuple>

// NOTE: converted from Python using ChatGPT
//       then fixed all errors/outdated syntax
// all versions exactly follow the algorithm used by d15.py to be comparable
// -> this C++ version could be optimized, e.g. by
//    - using a continguous 1D array for the grid
//    - same for visited with vec<bool>
//    - std::priority_queue for the queue instead of manual binsearch
// g++ -std=c++17 -O3 -march=native -o d15_cpp d15.cpp

struct Node {
    int level;
    int risk_sum;
    std::pair<int,int> prev;

    Node(int lvl, int risk) : level(lvl), risk_sum(risk), prev({-1,-1}) {}
};

using Pos = std::pair<int,int>;

// Custom hasher struct
struct PosHash {
    size_t operator()(const Pos& p) const noexcept {
        return std::hash<int>{}(p.first) ^ (std::hash<int>{}(p.second) << 1);
    }
};

// Binary search insertion index (descending cost queue)
size_t binsearch(const std::vector<std::pair<Pos,int>>& arr, int x) {
    if (arr.empty()) {
        return 0;
    }
    size_t first = 0;
    size_t last = arr.size() - 1;

    while (first < last) {
        size_t mid = first + (last - first) / 2;
        int val = arr[mid].second;
        if (val == x) return mid;
        else if (val > x) {
            if (first == mid) return (arr[last].second < x) ? last : last + 1;
            first = mid;
        } else {
            last = mid;
        }
    }
    return 0;
}

// Find shortest path using the same queue algorithm
int find_path(std::vector<std::vector<Node>>& grid, Pos start, Pos end_pos) {
    size_t dimy = grid.size();
    size_t dimx = grid[0].size();

    std::vector<std::pair<Pos,int>> q = { {start, 0} };
    // NOTE: unordered_set + custom hasher, since we need a hash-based set
    //       to be comparable with Python/Mojo
    // std::unordered_set<Pos, PosHash> visited{};
    std::set<Pos> visited{};
    visited.insert(start);

    while (!q.empty()) {
        auto [pos, path_cost] = q.back();
        q.pop_back();
        int x = pos.first;
        int y = pos.second;

        if (pos == end_pos) return path_cost;

        visited.insert(pos);

        const std::array<Pos, 4> neighbours{{
            {x, y-1}, {x+1, y}, {x, y+1}, {x-1, y} }};
        for (auto npos : neighbours) {
            int nx = npos.first;
            int ny = npos.second;
            if (nx < 0 || nx >= (int)dimx || ny < 0 || ny >= (int)dimy) continue;
            if (visited.count(npos)) continue;

            Node& neighbour_node = grid[ny][nx];
            int new_cost = path_cost + neighbour_node.level;
            if (new_cost < neighbour_node.risk_sum) {
                neighbour_node.risk_sum = new_cost;
                neighbour_node.prev = pos;

                size_t idx = binsearch(q, new_cost);
                q.insert(q.begin() + idx, {npos, new_cost});
            }
        }
    }
    return -1; // unreachable
}

int main() {
    std::ifstream infile("d15.in");
    std::string line;
    std::vector<std::string> contents;
    while (std::getline(infile, line)) {
        if (!line.empty()) contents.push_back(line);
    }

    // Build initial grid
    std::vector<std::vector<Node>> grid;
    for (auto& l : contents) {
        std::vector<Node> row;
        for (char c : l) {
            row.emplace_back(c - '0', std::numeric_limits<int>::max());
        }
        grid.emplace_back(std::move(row));
    }

    int part1 = find_path(grid, {0,0}, { (int)grid[0].size()-1, (int)grid.size()-1 });
    std::cout << "Part1: " << part1 << std::endl;

    // Build 5x expanded grid
    std::vector<std::vector<Node>> five_x_grid;
    for (int yy = 0; yy < 5; ++yy) {
        for (auto& l : contents) {
            std::vector<Node> row;
            for (int xx = 0; xx < 5; ++xx) {
                for (char c : l) {
                    int new_level = (c - '0' - 1 + xx + yy) % 9 + 1;
                    row.emplace_back(new_level, std::numeric_limits<int>::max());
                }
            }
            five_x_grid.emplace_back(std::move(row));
        }
    }

    auto start_time = std::chrono::high_resolution_clock::now();
    int part2 = find_path(five_x_grid, {0,0}, { (int)five_x_grid[0].size()-1, (int)five_x_grid.size()-1 });
    auto end_time = std::chrono::high_resolution_clock::now();

    std::cout << "Part2: " << part2 << std::endl;

    double took_ms = std::chrono::duration<double, std::milli>(end_time - start_time).count();
    std::cout << "Took: " << took_ms << " ms" << std::endl;

    return 0;
}
