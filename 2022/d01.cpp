#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>

int main() {
    std::ifstream infile("d01.in");
    std::vector<int> elves;

    std::string curline;
    int cursum = 0;
    int curmax = 0;
    int curmax_idx = 0;
    while (getline(infile, curline)) {
        std::cout << curline << '\n';
        if (curline == "") {
            if (cursum > curmax) {
                curmax = cursum;
                curmax_idx = elves.size();
            }
            elves.push_back(cursum);
            cursum = 0;
        } else {
            cursum += std::stoi(curline);
        }
    }

    std::cout << "Part 1: Elve " << curmax_idx + 1 << " with " << curmax << " rations\n";

    // use reverse iterators to sort in descending order
    std::sort(elves.rbegin(), elves.rend());
    std::cout << "Part 2: Top 3 elves carry " << std::accumulate(elves.begin(), elves.begin()+3, 0) << "\n";
}
