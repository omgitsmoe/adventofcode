#include <iostream>
#include <fstream>
#include <string>
#include <bitset>
#include <stdexcept>

// considered bad practice
using namespace std;


// NOTE: msvc only shows unhandled exceptions in the debugger
// otherwise the program just crashes
int main() {
    ifstream infile("d03.in");

    string curline;
    int prio_sum = 0;
    while (getline(infile, curline)) {
        if (curline.size() == 0) continue;

        int half_idx = curline.size() / 2;
        string compartment1 = curline.substr(0, half_idx);
        string compartment2 = curline.substr(half_idx, half_idx);
        // cout << compartment1 << endl << compartment2 << endl;
        // a-z prio: 1--26
        // A-Z prio: 27--52
        bitset<53> items; // only items in 1 technichally
        // better to use 2 sep loops but I wanted to test the list literal for loop
        for (const string &s : { compartment1, compartment2 }) {
            for (const char &c : s) {
                int idx = c <= static_cast<int>('Z') ?
                    c - static_cast<int>('A') + 26 :
                    c - static_cast<int>('a');
                // cout << "Char '" << c << "' -> idx: " << idx << endl;
                // letters can be duplicate per compartment, so only want to set them in compartment1
                // and check them in 2
                // where we also need to reset them so they can't be counted more than once
                // ... this really would've been better with sep loops or using a normal set
                // or use two bitsets instead and and them together
                if (items.test(idx) && s == compartment2) {
                    // cout << "Duplicate " << c << " value " << idx + 1 << endl;
                    prio_sum += idx + 1;
                    // reset so we don't count it a 2nd time
                    items.set(idx, false);
                } else if (s == compartment1) {
                    items.set(idx);
                }
            }
        }
    }

    // part2
    // return to beginning of file and clear eof flags etc.
    infile.clear();
    infile.seekg(0);

    // start at 1 so we don't hit 0%3==0 instantly
    int idx = 1;
    int common_prio_sum = 0;
    bitset<53> rucksacks[3];
    while (getline(infile, curline)) {
        if (curline.size() == 0) continue;

        // make sure this is a reference or use a pointer!
        auto &items = rucksacks[idx % 3];
        for (const char &c : curline) {
            int prio = c <= static_cast<int>('Z') ?
                c - static_cast<int>('A') + 26 :
                c - static_cast<int>('a');
            items.set(prio);
        }

        if ((idx % 3) == 0) {
            auto result = (rucksacks[0] & rucksacks[1]) & rucksacks[2];
            for (size_t i = 0; i < result.size(); ++i) {
                if (result[i]) {
                    common_prio_sum += i + 1;
                }
            }

            // reset the 3 bitsets
            rucksacks[0].reset();
            rucksacks[1].reset();
            rucksacks[2].reset();
        }
        idx += 1;
    }

    cout << "Part1: Duplicate items prio sum is " << prio_sum << endl;
    cout << "Part2: Triple items prio sum is " << common_prio_sum << endl;
}
