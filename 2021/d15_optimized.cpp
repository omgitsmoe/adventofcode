#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <limits>
#include <queue>
#include <chrono>

// NOTE: converted from Python using ChatGPT
//       then fixed all errors/outdated syntax
// WARN: this version only roughly follows the algorithm used by d15.py
// -> this C++ version is optimized, by
//    - using a continguous 1D array for the grid
//    - same for visited with vec<bool>
// g++ -std=c++17 -O3 -march=native -o d15_optimized_cpp d15_optimized.cpp
// -> without I/O and only part2 ~16.34ms, with I/O etc. ~19.3ms

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <limits>
#include <chrono>

struct Node {
    int level;
    int risk_sum;
    int prev_index;

    Node(int lvl) : level(lvl), risk_sum(std::numeric_limits<int>::max()), prev_index(-1) {}
};

// Convert 2D coordinates to 1D index
inline int idx(int x, int y, int width) {
    return y * width + x;
}

// Binary search insertion index (descending cost queue)
size_t binsearch(const std::vector<std::pair<int,int>>& arr, int x) {
    if (arr.empty()) return 0;
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

// Find shortest path using vector + binary search queue
int find_path(std::vector<Node>& grid, int width, int height, int start_idx, int end_idx) {
    std::vector<bool> visited(width * height, false);

    // Queue stores (index, risk_sum)
    std::vector<std::pair<int,int>> q;
    grid[start_idx].risk_sum = 0;
    q.emplace_back(start_idx, 0);

    const int dx[4] = {0, 1, 0, -1};
    const int dy[4] = {-1, 0, 1, 0};

    while (!q.empty()) {
        auto [index, path_cost] = q.back();
        q.pop_back();

        if (visited[index]) continue;
        visited[index] = true;

        if (index == end_idx) return path_cost;

        int x = index % width;
        int y = index / width;

        for (int d = 0; d < 4; ++d) {
            int nx = x + dx[d];
            int ny = y + dy[d];
            if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue;

            int nidx = idx(nx, ny, width);
            if (visited[nidx]) continue;

            int new_cost = path_cost + grid[nidx].level;
            if (new_cost < grid[nidx].risk_sum) {
                grid[nidx].risk_sum = new_cost;
                grid[nidx].prev_index = index;

                size_t insert_idx = binsearch(q, new_cost);
                q.insert(q.begin() + insert_idx, {nidx, new_cost});
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

    int height = contents.size();
    int width = contents[0].size();

    // Flattened grid
    std::vector<Node> grid;
    grid.reserve(width * height);
    for (auto& l : contents) {
        for (char c : l) {
            grid.emplace_back(c - '0');
        }
    }

    int part1 = find_path(grid, width, height, 0, idx(width-1, height-1, width));
    std::cout << "Part1: " << part1 << std::endl;

    // Part2: expand 5x
    int new_width = width * 5;
    int new_height = height * 5;
    std::vector<Node> five_x_grid;
    five_x_grid.reserve(new_width * new_height);

    for (int yy = 0; yy < 5; ++yy) {
        for (int y = 0; y < height; ++y) {
            for (int xx = 0; xx < 5; ++xx) {
                for (int x = 0; x < width; ++x) {
                    int level = (contents[y][x] - '0' - 1 + xx + yy) % 9 + 1;
                    five_x_grid.emplace_back(level);
                }
            }
        }
    }

    auto start_time = std::chrono::high_resolution_clock::now();
    int part2 = find_path(five_x_grid, new_width, new_height, 0, idx(new_width-1, new_height-1, new_width));
    auto end_time = std::chrono::high_resolution_clock::now();

    std::cout << "Part2: " << part2 << std::endl;
    double took_ms = std::chrono::duration<double, std::milli>(end_time - start_time).count();
    std::cout << "Took: " << took_ms << " ms" << std::endl;

    return 0;
}

// version below is additionally optimized using std::priority_queue
// instead of manual binsearch
// g++ -std=c++17 -O3 -march=native -o d15_optimized_cpp d15_optimized.cpp
// -> without I/O and only part2 ~12.7ms, with I/O etc. ~15.8ms
/*
struct Node {
    int level;
    int risk_sum;
    std::pair<int,int> prev;

    Node(int lvl) : level(lvl), risk_sum(std::numeric_limits<int>::max()), prev({-1,-1}) {}
};

using Pos = std::pair<int,int>;

struct QueueNode {
    int risk_sum;
    Pos pos;

    bool operator<(const QueueNode& other) const {
        // Min-heap
        return risk_sum > other.risk_sum;
    }
};

// Convert 2D coordinates to 1D index
inline int idx(int x, int y, int width) {
    return y * width + x;
}

// Dijkstra with flattened grid + priority queue + visited vector
int find_path(std::vector<Node>& grid, int width, int height, Pos start, Pos end_pos) {
    std::vector<bool> visited(width * height, false);

    std::priority_queue<QueueNode> pq;
    grid[idx(start.first, start.second, width)].risk_sum = 0;
    pq.push({0, start});

    const int dx[4] = {0, 1, 0, -1};
    const int dy[4] = {-1, 0, 1, 0};

    while (!pq.empty()) {
        auto current = pq.top(); pq.pop();
        int x = current.pos.first;
        int y = current.pos.second;
        int index = idx(x, y, width);

        if (visited[index]) continue;
        visited[index] = true;

        if (current.pos == end_pos) {
            return grid[index].risk_sum;
        }

        for (int d = 0; d < 4; ++d) {
            int nx = x + dx[d];
            int ny = y + dy[d];
            if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue;

            int nidx = idx(nx, ny, width);
            Node& neighbor = grid[nidx];
            if (visited[nidx]) continue;

            int new_cost = grid[index].risk_sum + neighbor.level;
            if (new_cost < neighbor.risk_sum) {
                neighbor.risk_sum = new_cost;
                neighbor.prev = {x, y};
                pq.push({new_cost, {nx, ny}});
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

    int height = contents.size();
    int width = contents[0].size();

    // Flattened grid
    std::vector<Node> grid;
    grid.reserve(width * height);
    for (auto& l : contents) {
        for (char c : l) {
            grid.emplace_back(c - '0');
        }
    }

    int part1 = find_path(grid, width, height, {0,0}, {width-1, height-1});
    std::cout << "Part1: " << part1 << std::endl;

    // Part2: expand 5x
    int new_width = width * 5;
    int new_height = height * 5;
    std::vector<Node> five_x_grid;
    five_x_grid.reserve(new_width * new_height);

    for (int yy = 0; yy < 5; ++yy) {
        for (int y = 0; y < height; ++y) {
            for (int xx = 0; xx < 5; ++xx) {
                for (int x = 0; x < width; ++x) {
                    int level = (contents[y][x] - '0' - 1 + xx + yy) % 9 + 1;
                    five_x_grid.emplace_back(level);
                }
            }
        }
    }

    auto start_time = std::chrono::high_resolution_clock::now();
    int part2 = find_path(five_x_grid, new_width, new_height, {0,0}, {new_width-1, new_height-1});
    auto end_time = std::chrono::high_resolution_clock::now();

    std::cout << "Part2: " << part2 << std::endl;
    double took_ms = std::chrono::duration<double, std::milli>(end_time - start_time).count();
    std::cout << "Took: " << took_ms << " ms" << std::endl;

    return 0;
}
*/

