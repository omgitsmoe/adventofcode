#include <fstream>
#include <iostream>
#include <string>
#include <set>
#include <string_view>
#include <charconv>
#include <algorithm>

#include "utils.cpp"

struct Point { int y, x; };

// /std:c++17
int main() {
    std::ifstream infile("d14.in");
    std::string line;
    auto cmp = [](Point a, Point b) { return (a.y < b.y) || (a.y == b.y && a.x < b.x); };
    std::set<Point, decltype(cmp)> occupied(cmp);

    while (getline(infile, line)) {
        SplitIter iter(line, " -> ", 0);
        std::string_view coord_str;
        bool first = true;
        int last_x, last_y;
        while (iter.more()) {
            iter.next_view(coord_str);
            size_t x_end = coord_str.find(",");
            int x, y;
            auto result = std::from_chars(coord_str.data(), coord_str.data() + x_end, x);
            result = std::from_chars(
                coord_str.data() + x_end + 1, coord_str.data() + coord_str.size(), y);
            if (!first) {
                // add all the points in the path to the occupied set
                int low_x = last_x < x ? last_x : x;
                int low_y = last_y < y ? last_y : y;
                int high_x = last_x > x ? last_x : x;
                int high_y = last_y > y ? last_y : y;
                for (int yy = low_y; yy <= high_y; yy += 1) {
                    for (int xx = low_x; xx <= high_x; xx += 1) {
                        occupied.insert({ yy, xx });
                    }
                }
            }
            last_x = x;
            last_y = y;
            first = false;
        }
    }

    int ymax = 0, xmax = 0;
    for (auto p : occupied) {
        // std::cout << "P x" << p.x << " y" << p.y << std::endl;
        if (p.y > ymax) ymax = p.y;
        if (p.x > xmax) xmax = p.x;
    }
    const int floor = 2 + ymax; // PART2

    // drop sand from 500x0y
    int sx = 500;
    int sy = 0;
    int sand_units = 0;
    auto occupied_copy = occupied;
    while (true) {
        // sand move prio: down | down-left | down-right
        if (sy > ymax) {
            // falling into abyss since beyond any rock wall
            break;
        } else if (occupied_copy.find({ sy + 1, sx }) == occupied_copy.end()) {
            // down
            sy += 1;
        } else if (occupied_copy.find({ sy + 1, sx - 1 }) == occupied_copy.end()) {
            // down-left
            sy += 1;
            sx -= 1;
        } else if (occupied_copy.find({ sy + 1, sx + 1 }) == occupied_copy.end()) {
            // down-right
            sy += 1;
            sx += 1;
        } else {
            // not occupied_copy -> come to rest here
            occupied_copy.insert({sy, sx});
            sand_units += 1;
            // drop from start again
            sx = 500;
            sy = 0;
        }
    }

    std::cout << "Part1: Sand units that have come to rest before abyss: " << sand_units << std::endl;

    sx = 500;
    sy = 0;
    sand_units = 0;
    while (true) {
        // sand move prio: down | down-left | down-right
        bool rest = false;
        if (sy == (floor - 1)) {
            // reached cave floor
            rest = true;
        } else if (occupied.find({ sy + 1, sx }) == occupied.end()) {
            // down
            sy += 1;
        } else if (occupied.find({ sy + 1, sx - 1 }) == occupied.end()) {
            // down-left
            sy += 1;
            sx -= 1;
        } else if (occupied.find({ sy + 1, sx + 1 }) == occupied.end()) {
            // down-right
            sy += 1;
            sx += 1;
        } else {
            rest = true;
        }

        if (rest) {
            // not occupied -> come to rest here
            occupied.insert({sy, sx});
            sand_units += 1;
            if (sy == 0 && sx == 500) {
                // came to rest at src
                break;
            }
            // drop from start again
            sx = 500;
            sy = 0;
        }
    }

    std::cout << "Part2: Sand units that have come to rest before src is blocked: " << sand_units << std::endl;
}
