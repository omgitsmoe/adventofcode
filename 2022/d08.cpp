#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <stdint.h>
#include <algorithm>

void count_visible(std::vector<uint8_t>& trees, std::vector<bool>& visible, int start, int steps, int step) {
    // NOTE: 0 is an actual tree height.........
    int max = -1;
    for (int i = 0; i < steps; ++i) {
        size_t index = start + step * i;
        auto val = trees.at(index);
        if (static_cast<int>(val) > max) {
            // std::cout << "visible at " << index << "; r" << index / 5 << "c" << index % 5 << std::endl;
            visible[index] = true;
            max = val;
        }
    }
}

int scenic_score(std::vector<uint8_t>& trees, int start, int step, int stop) {
    auto height = trees[start];
    int sum = 1;
    int index = start + step;
    // std::cout << "start " << start << " step " << step << " stop " << stop << std::endl;
    while (index != stop) {
        auto val = trees.at(index);
        if (height > val) {
            sum += 1;
        } else {
            // std::cout << "stopped early at " << index << " shouldve at " << stop << std::endl;
            // if ((step > 0 && index > stop) || (step < 0 && index < stop))
            //     std::cout << "FUCKED UP" << std::endl;
            break;
        }

        index += step;
    }

    return sum;
}

int main() {
    std::ifstream infile("d08.in");
    std::string line;
    std::vector<uint8_t> trees;
    char c;
    int32_t cols = -1;
    while (infile.get(c)) {
        if (c >= '0' && c <= '9') {
            trees.push_back(c - '0');
        } else if (cols == -1 && (c == '\r' || c == '\n')) {
            // get cols when hitting first line break
            cols = trees.size();
        }
    }

    int32_t rows = trees.size() / cols;
    std::cout << "Rows " << rows << " Cols " << cols << std::endl;

    // count edges as visible
    // size_t visible = col * 2 + (row - 1) * 2;
    // same dim as trees with visible set to false
    std::vector<bool> visible(trees.size(), false);

#define DBG_IDX(idx) std::cout << "idx r" << idx / cols << "c" << idx % cols << std::endl;

    for (int32_t col = 0; col < cols; ++col) {
        // forwards
        // start at col, we need to step down all rows, and one step/stride is the nr of cols
        count_visible(trees, visible, col, rows, cols);
        // and backwards (from the right instead, so it's easier, otherwise -cols + col)
        count_visible(trees, visible, trees.size() - col - 1, rows, -cols);
    }
    for (int32_t row = 0; row < rows; ++row) {
        // from left
        // start at the row index * cols since we cols amount of items are between each row
        // to iterate the whole row we have cols steps
        count_visible(trees, visible, row * cols, cols, 1);
        // from right
        // same as above but start at right so + cols - 1
        count_visible(trees, visible, row * cols + cols - 1, cols, -1);
    }

// NOTE: paren around sub-expressions are important otherwise operator precedence can bite you
#define INDEX(row, col) ((row) * (cols) + (col))
    size_t num_visible = 0;
    for (auto v : visible) if (v) num_visible += 1;

    // show grids of visibles (1 visible 0 hidden)
    // size_t idx = 0;
    // for (auto v : visible) {
    //     if (v) {
    //         std::cout << "1";
    //     } else {
    //         std::cout << "0";
    //     }
    //     idx += 1;
    //     if ((idx % cols) == 0) std::cout << std::endl;
    // }


    std::cout << "Part1: Visible trees " << num_visible << std::endl;

    std::vector<int> scores;
    scores.reserve(trees.size());
    // excluding edges, assuming they can't have the highest scenic score
    // which means we have to do less range checking
    for (size_t row = 1; row < (rows - 1); ++row) {
        for (size_t col = 1; col < (cols - 1); ++col) {
            size_t index = INDEX(row, col);
            uint32_t scenic = 1;
            // right
            scenic *= scenic_score(trees, index, 1, INDEX(row, cols - 1));
            // down
            scenic *= scenic_score(trees, index, cols, INDEX(rows - 1, col));
            // left
            scenic *= scenic_score(trees, index, -1, INDEX(row, 0));
            // up
            scenic *= scenic_score(trees, index, -cols, INDEX(0, col));
            // std::cout << scenic << " r" << row <<"c"<<col<<std::endl;

            scores.push_back(scenic);
        }
    }

    std::cout << "Part2: Max scenic score is " << *std::max_element(scores.begin(), scores.end()) << std::endl;
}
