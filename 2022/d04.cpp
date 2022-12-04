#include <iostream>
#include <fstream>
#include <string>

struct Range {
    int start;
    int end_inclusive;
};

bool contains_range(Range a, Range b) {
    bool ret = false;
    if ((a.start <= b.start) &&
            (b.start <= a.end_inclusive) &&
            (a.end_inclusive >= b.end_inclusive)) {
        ret = true;
        // std::cout << "A: " << a.start << "-" << a.end_inclusive <<
        //     "; B: " << b.start << "-" << b.end_inclusive << std::endl;
    }

    return ret;
}

bool overlaps(Range a, Range b) {
    bool ret = true;
    // easier to check if there's no overlap
    //  a  b
    // 123...
    // ...455
    //  b  a
    // 123...
    // ...455
    if (a.start > b.end_inclusive) {
        // starts after b
        ret = false;
    } else if (a.start < b.start && a.end_inclusive < b.start) {
        // start+ends before b
        ret = false;
    }
    // std::cout << "A: " << a.start << "-" << a.end_inclusive <<
    //     "; B: " << b.start << "-" << b.end_inclusive << std::endl;

    return ret;
}

Range range_from_str(const std::string& str) {
    Range result;
    int dash_idx = str.find("-");

    result.start = std::stoi(str.substr(0, dash_idx));
    result.end_inclusive = std::stoi(str.substr(dash_idx + 1, str.size() - dash_idx));

    return result;
}

// NOTE: msvc only shows unhandled exceptions in the debugger
// otherwise the program just crashes
int main() {
    std::ifstream infile("d04.in");

    std::string curline;
    int fully_contained = 0;
    int num_overlaps = 0;
    while (std::getline(infile, curline)) {
        if (curline.size() == 0) continue;
        int sep_idx = curline.find(",");
        Range range1 = range_from_str(curline.substr(0, sep_idx));
        Range range2 = range_from_str(curline.substr(sep_idx + 1, curline.size() - sep_idx));
        if (contains_range(range1, range2) || contains_range(range2, range1)) {
            fully_contained += 1;
        }
        if (overlaps(range1, range2)) {
            num_overlaps += 1;
        }
    }
    std::cout << "Part1: Fully contained ranges " << fully_contained << std::endl;
    std::cout << "Part2: Overlaps " << num_overlaps << std::endl;
}
