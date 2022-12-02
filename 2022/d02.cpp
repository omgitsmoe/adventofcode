#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>
#include <tuple>

using namespace std;

typedef tuple<char, char> Round;

#define WIN 6
#define DRAW 3
#define LOSE 0

char get_option(char in) {
    char ret;
    switch (in) {
        case 'A':
        case 'X':
            ret = 1;
            break;
        case 'B':
        case 'Y':
            ret = 2;
            break;
        case 'C':
        case 'Z':
            ret = 3;
            break;
    }

    return ret;
}

// look up table for outcomes/points
// base is for me having rock
// indices for paper and scissors need to be shifted by 1 and by 2 respectively
// rock: rock, paper, scissors
// rock 0
// paper +1
// scissors +2
// me rock: o0m0 D; o1m0 L; o2m0 W;
// me paper: o0m1 +1 W; o1m1 +1 D; o2m1 +1 L;
// me sciss: o0m2 +2 L; o1m2 +2 W; o2m2 +2 D;
char POINT_LUT[3] = {DRAW, LOSE, WIN};
// index shift into LUT for rock, paper, scissors
char IDX_SHIFT[3] = {0, 1, 2};

// char[][3] = {
//     // rock
//     // rock, paper, scissors
//     {DRAW, LOSE, WIN},
//     // paper
//     // rock, paper, scissors
//     {WIN, DRAW, LOSE},
//     // scissors
//     // rock, paper, scissors
//     {LOSE, WIN, DRAW},
// };

int main() {
    ifstream infile("d02.in");
    vector<Round> rounds;

    string curline;
    int points = 0;
    int points2 = 0;
    while (getline(infile, curline)) {
        if (curline.size() == 0) continue;
        // cout << curline << '\n';
        char opponent = get_option(curline[0]);
        char me = get_option(curline[2]);
        // cout << "Opp: " << +opponent << " Me: " << +me << "\n";
        // look-up outcome points in LUT shifting look-up index by the option we played
        // int outcome = POINT_LUT[((me - 1) + IDX_SHIFT[me - 1] + (opponent - 1)) % 3];
        // not needed ^
        char idx = (opponent - me) % 3;
        // when one modulo opperand is negative whether the outcome is neg. is implemenation defined
        int outcome = POINT_LUT[idx < 0 ? idx + 3 : idx];
        points += outcome;
        // points for the chosen option
        points += me;
        // cout << "O " << outcome << " + " << +me << "\n";

        // part2
        // if opponent chooses rock:
        // need to choose same to DRAW; +1 (so paper) to WIN; +2 (so scissors) to LOSE;
        // if opponent chooses paper:
        // need to choose same to DRAW; +1 (so scissors) to WIN; +2 (so rock) to LOSE;
        char needed_outcome;
        char outcome_shift;
        switch (curline[2]) {
            case 'X':
                needed_outcome = LOSE;
                // need opponent to WIN
                outcome_shift = 2;
                break;
            case 'Y':
                needed_outcome = DRAW;
                outcome_shift = 0;
                break;
            case 'Z':
                needed_outcome = WIN;
                // need opponent to LOSE
                outcome_shift = 1;
                break;
        }

        // opponent - 1 to make it 0-based
        char needed_me = 1 + (outcome_shift + opponent - 1) % 3;
        points2 += needed_outcome + needed_me;
        cout << "O " << +needed_outcome << " + " << +needed_me << "\n";
    }

    cout << "Part1: Total score is " << points << "\n";
    cout << "Part2: Total score is " << points2 << "\n";
}
