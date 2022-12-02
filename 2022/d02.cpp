#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>
#include <tuple>

using namespace std;

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
// rock: rock, paper, scissors
char POINT_LUT[3] = {DRAW, LOSE, WIN};

int main() {
    ifstream infile("d02.in");

    string curline;
    int points = 0;
    int points2 = 0;
    while (getline(infile, curline)) {
        if (curline.size() == 0) continue;
        // cout << curline << '\n';
        char opponent = get_option(curline[0]);
        char me = get_option(curline[2]);
        // cout << "Opp: " << +opponent << " Me: " << +me << "\n";
        // distance from each other determines 0=D/1=L/2=W
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
