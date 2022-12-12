#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <set>
#include <vector>

int clamp(int v, int low, int high) {
    // return std::max(low, std::min(v, high));
    return v >= low ? (v <= high ? v : high) : low;
}

struct Point { int y, x; };
struct Step { int dy, dx; int num_steps; };

void follow(const Point& head, Point& tail) {
    int dy = head.y - tail.y;
    int dx = head.x - tail.x;
    int manhattan_dist = std::abs(dy) + std::abs(dx);
    //std::cout << "P y"<<tail.y<<"x"<<tail.x<<std::endl;
    if (manhattan_dist <= 1) {
        // still touching
        return;
    }

    bool row_match = head.y == tail.y;
    bool col_match = head.x == tail.x;
    if (row_match || col_match) {
        // follow in cardinal direction
        if (col_match) {
            tail.y += dy > 0 ? 1 : -1;
        } else {
            tail.x += dx > 0 ? 1 : -1;
        }
    } else {
        if (manhattan_dist <= 2) {
            // still touching diagonally
            return;
        }
        // follow diagonally
        // moving a max of 2 (1 in each direction)
        tail.y += clamp(dy, -1, 1);
        tail.x += clamp(dx, -1, 1);
    }
}

int main() {
    std::ifstream infile("d09.in");
    std::string line;
    std::vector<Step> step_buf;
    int hy = 0, hx = 0;
    int ty = 0, tx = 0;
    // custom compare function
    // forgot the a.y==b.y condition in the comparison so my set count was wrong
    // even though my algo was correct and I was searching for errors in my algo for ever... :/
    // auto cmp = [](Point a, Point b) { return a.y < b.y ? true : (a.y == b.y ? (a.x < b.x) : false); };
    // both work ^ v
    auto cmp = [](Point a, Point b) { return (a.y < b.y) || (a.y == b.y && a.x < b.x); };
    std::set<Point, decltype(cmp)> visited(cmp);
    visited.insert({ ty, tx });

    while (getline(infile, line)) {
        if (line.size() == 0) continue;
        char dir = line[0];
        int head_dy = 0, head_dx = 0;
        switch (dir) {
            case 'U':
                head_dy = -1;
                break;
            case 'D':
                head_dy = 1;
                break;
            case 'R':
                head_dx = 1;
                break;
            case 'L':
                head_dx = -1;
                break;
        }

        int steps = std::stoi(line.substr(2));
        // store for part2
        step_buf.push_back(Step{ head_dy, head_dx, steps });

        for (int i = 0; i < steps; ++i) {
            // move the head
            hy += head_dy;
            hx += head_dx;

            // offset in y/x direction
            int dy = hy - ty;
            int dx = hx - tx;
            int manhattan_dist = std::abs(dy) + std::abs(dx);
            //std::cout << "P y"<<ty<<"x"<<tx<<std::endl;
            if (manhattan_dist <= 1) {
                // still touching
                continue;
            }

            bool row_match = hy == ty;
            bool col_match = hx == tx;
            if (row_match || col_match) {
                // follow in cardinal direction
                if (col_match) {
                    ty += dy > 0 ? 1 : -1;
                } else {
                    tx += dx > 0 ? 1 : -1;
                }
            } else {
                if (manhattan_dist <= 2) {
                    // still touching diagonally
                    continue;
                }
                // follow diagonally
                // moving a max of 2 (1 in each direction)
                ty += clamp(dy, -1, 1);
                tx += clamp(dx, -1, 1);
            }
            // mark as visited
            visited.insert({ ty, tx });
        }
    }

    std::cout << "h " << hy << "," << hx << " t " << ty << "," << tx << std::endl;
    std::cout << "Part1: Visited at least once: " << visited.size() << std::endl;

    // init to 0 with {} ({0} technically only says init first element to 0 and
    // only works due to  aggregate initialization (get default inited)
    // but {0} works in both C++ and C
    Point knots[10] = {};
    visited.clear();
    for (const auto& step : step_buf) {
        for (int step_num = 0; step_num < step.num_steps; ++step_num) {
            auto& head = knots[0];
            head.y += step.dy;
            head.x += step.dx;

            for (int knot_idx = 1; knot_idx < 10; ++knot_idx) {
                auto& knot = knots[knot_idx];
                // follow preceding knot
                auto& follow_knot = knots[knot_idx - 1];
                follow(follow_knot, knot);
            }
            // mark the pos of last tail as visited
            visited.insert(knots[9]);
        }
    }

    std::cout << "Part2: Visited at least once: " << visited.size() << std::endl;
}
