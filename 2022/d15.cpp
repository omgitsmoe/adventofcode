#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <charconv>
#include <cassert>
#include <set>
#include <cstdio>

struct Point { int y, x; };
struct Sensor {
    Point pos;
    Point closest_beacon;
};

#define MANHATTAN(p1, p2) (std::abs((p1).x - (p2).x) + std::abs((p1).y - (p2).y))
bool has_covered(const Sensor& sensor, Point p) {
    // int dist_x = std::abs(sensor.pos.x - sensor.closest_beacon.x);
    // int dist_y = std::abs(sensor.pos.y - sensor.closest_beacon.y);
    // int minx = sensor.pos.x - dist_x;
    // int miny = sensor.pos.y - dist_y;
    // int maxx = sensor.pos.x + dist_x;
    // int maxy = sensor.pos.y + dist_y;
    // if (p.x < minx || p.x > maxx || p.y < miny || p.y > maxy) {
    //     return false;
    // } else {
    //     return true;
    // }
    if (MANHATTAN(sensor.pos, p) <= MANHATTAN(sensor.pos, sensor.closest_beacon)) {
        return true;
    } else {
        return false;
    }
}

bool any_covered(const std::vector<Sensor>& ss, Point p) {
    for (const auto& s : ss) {
        // AND THEN.... excluding beacons like below is not needed anymore for part2......
        // .......................
        if (has_covered(s, p)) {
            return true;
        }
    }

    return false;
}

bool any_covered_part1(const std::vector<Sensor>& ss, Point p) {
    for (const auto& s : ss) {
        if ((s.closest_beacon.x == p.x) && (s.closest_beacon.y == p.y)) {
            // how many postitions cannot contain a beacon?
            // I thought it meant unknown beacons.. so known closest beacons would count towards it
            // BUT IT DOES NOT INCLUDE the existing closest beacons (since you can't place any more there
            // because it was excluded by the rule of there not being beacones with the same
            // manhattan_dist to sensors)
            // SO...... remove the beacons from the positions
            return false;
        } else if (has_covered(s, p)) {
            return true;
        }
    }

    return false;
}

int main() {
    std::ifstream infile("d15.in");
    std::string line;
    std::vector<Sensor> sensors;
    while (getline(infile, line)) {
        int sensor_x_start = 12;
        // exclusive
        int sensor_x_end = line.find(",", sensor_x_start);
        int sensor_y_start = sensor_x_end + 4;
        int sensor_y_end = line.find(":", sensor_y_start);
        int beacon_x_start = line.find("=", sensor_y_end) + 1;
        int beacon_x_end = line.find(",", beacon_x_start);
        int beacon_y_start = beacon_x_end + 4;
        Sensor s;
        // std::cout << line[sensor_x_start] << " xe " << line[sensor_x_end] << " sys " << line[sensor_y_start] << " sye " << line[sensor_y_end] << " bxs " << line[beacon_x_start] << " bxe " << line[beacon_x_end] << " bys " << line[beacon_y_start] << std::endl;
        std::from_chars(&line[sensor_x_start], &line[sensor_x_end], s.pos.x);
        std::from_chars(&line[sensor_y_start], &line[sensor_y_end], s.pos.y);
        std::from_chars(&line[beacon_x_start], &line[beacon_x_end], s.closest_beacon.x);
        std::from_chars(&line[beacon_y_start], &line[line.size()], s.closest_beacon.y);
        // std::cout << "Sensor: sx" << s.pos.x << "sy" << s.pos.y << " bx" << s.closest_beacon.x << "by" << s.closest_beacon.y << std::endl;
        sensors.push_back(s);
    }

    // shows coverage of one test sensor
    // Sensor test { Point {7,8}, Point {10,2} };
    // for (int y = -3; y < 18; ++y) {
    //     for (int x = -3; x < 19; ++x) {
    //         Point p = {y,x};
    //         if (p.x == 8 && p.y == 7) {
    //             std::cout << "S";
    //         } else if (p.x == 2 && p.y == 10) {
    //             std::cout << "B";
    //         } else {
    //             std::cout << (has_covered(test, p) ? "#" : ".");
    //         }
    //     }
    //     std::cout << std::endl;
    // }

    Point min = { INT_MAX, INT_MAX };
    Point max = { 0, 0 };
    for (const auto& s : sensors) {
        int manhattan_dist = MANHATTAN(s.pos, s.closest_beacon);
        // below not enough since it can go the whole manhattan_dist in one direction (x/y)
        // int dist_x = std::abs(s.pos.x - s.closest_beacon.x);
        // int dist_y = std::abs(s.pos.y - s.closest_beacon.y);
        int minx = s.pos.x - manhattan_dist;
        int miny = s.pos.y - manhattan_dist;
        int maxx = s.pos.x + manhattan_dist;
        int maxy = s.pos.y + manhattan_dist;
        if (minx < min.x) min.x = minx;
        if (maxx > max.x) max.x = maxx;
        if (miny < min.y) min.y = miny;
        if (maxy > max.y) max.y = maxy;
    }

    // prob too many rows/cols to cover in a grid
    // better to just iterate over sensors and ask if the coordinates are covered by it
    // depending on the closest beacon found

    // 10 for example; 2000000 for real input
    const size_t TEST_ROW = 2000000;
    size_t covered = 0;
    for (int x = min.x; x <= max.x; ++x) {
        if (any_covered_part1(sensors, { TEST_ROW, x })) {
            covered += 1;
        }
    }

    // prints test row with 2 rows of context
    // std::cout << "start " << min.x << "," << TEST_ROW << std::endl;
    // for (int y = TEST_ROW - 2; y <= (TEST_ROW + 2); ++y) {
    //     printf("%02d", y);
    //     for (int x = min.x; x <= max.x; ++x) {
    //         // if (covered.count({ y, x }) > 0) {
    //         if (any_covered(sensors, {y,x})) {
    //             std::cout << "#";
    //         } else {
    //             std::cout << ".";
    //         }
    //     }
    //     std::cout << std::endl;
    // }

    std::cout << "Part1: Postitions covered in row 2 000 000: " << covered << std::endl;

    int distress_min = 0;
    // real 4000000 example 20
    int distress_max = 4000000;
    // guaranteed >=0 so we can use size_t
#define TUNING_FREQ(y, x) (((size_t)x) * 4000000 + ((size_t)y))

    // NOTE: since we know there is only __one__ beacon position that is not covered by the
    // sensor range (which have a diamond shape), we know that the only open spot
    // has to be directly next to the border of a sensor's diamond range
    // -> walk the border, or rather the positions adjacent to the border and only check
    //    them for coverage
    for (const auto& s : sensors) {
        // use manhattan_dist + 1 so we actually check the neighbour points of the borders
        // ...X...
        // ..x#x..
        // .x###x. instead of dist 1 use two to start at X
        // ..x#x.. then we basically walk the border of a diamond that is two (or 1 to one side) larger
        // ...x... at the widest points
        int manhattan_dist = MANHATTAN(s.pos, s.closest_beacon) + 1;
        // walk the border of the (range + 1) covered by the sensor
        for (int yoffset = -manhattan_dist; yoffset < manhattan_dist; ++yoffset) {
            // offset (width of coverage) decreases as we go farther from the sensor pos
            // (0 at the farthest points)
            int xoffset = manhattan_dist - std::abs(yoffset);
            // check right and left side
            for (int i = 0; i < 2; ++i, xoffset *= -1) {
                Point p = { s.pos.y + yoffset, s.pos.x + xoffset };
                if ((p.x >= 0) && (p.y >= 0) && (p.x <= distress_max) && (p.y <= distress_max) &&
                        !any_covered(sensors, p)) {
                    std::cout << "Part2: Only possible location for the distress beacon: " << p.x
                        << "," << p.y << " -> TuningFreq: " << TUNING_FREQ(p.y, p.x) << std::endl;
                }
            }
        }
    }
}
