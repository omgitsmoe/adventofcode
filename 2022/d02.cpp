#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>
#include <tuple>

using namespace std;


int main() {
    ifstream infile("d02.in");

    string curline;
    int points = 0;
    int points2 = 0;
    while (getline(infile, curline)) {
        if (curline.size() == 0) continue;
        // cout << curline << '\n';
        char opponent = curline[0] - 'A';
        char me = curline[2] - 'X';
        // cout << "Opp: " << +opponent << " Me: " << +me << "\n";
        // 0 rock 1 paper 2 scissors
        // base needs to be from opponents view so we have D/L/W (instead of D/W/L)
        // -> shift by one so we get 0=L/1=D/2=W
        char offset = me - opponent + 1;
        // when one modulo opperand is negative whether the outcome is neg. is implemenation defined
        offset = offset >= 0 ? offset % 3 : offset + 3;
        int outcome_points = offset * 3;
        points += outcome_points;
        // points for the chosen option
        points += me + 1;
        // cout << "O " << outcome << " + " << +(me + 1) << "\n";

        // part2
        // 0 loss 1 draw 2 win
        char needed_outcome = me;
        // since opp==me is normally the start (draw) we need to shift by one to the left
        // normally 0 draw 1 win 2 loss
        // shifted 0 loss 1 draw 2 win
        char outcome_shift = opponent - 1 + needed_outcome;
        char needed_me = outcome_shift >= 0 ? outcome_shift % 3 : outcome_shift + 3;

        points2 += needed_outcome * 3 + needed_me + 1;
        // cout << "O " << +needed_outcome << " + " << +needed_me << "\n";
    }

    cout << "Part1: Total score is " << points << "\n";
    cout << "Part2: Total score is " << points2 << "\n";
}
