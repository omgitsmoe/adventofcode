#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

constexpr int NUM_COLS = 9;

// paren needed around name to mark it with & as ref
void print_stacks(const std::vector<char> (&stacks)[NUM_COLS]) {
    int max = 0;
    for (int i = 0; i < NUM_COLS; ++i) {
        int size = stacks[i].size();
        if (size > max) {
            max = size;
        }
    }

    for (int row = max - 1; row >= 0; row--) {
        for (int i = 0; i < NUM_COLS; ++i) {
            // std::cout << "r" << row << "c" << i << " rmax " << stacks[i].size() << std::endl;
            if (row > (stacks[i].size() - 1)) {
                std::cout << "    ";
            } else {
                std::cout << "[" << stacks[i][row] << "] ";
            }
        }

        std::cout << std::endl;
    }
    for (int i = 0; i < NUM_COLS; ++i) {
        std::cout << " " << i+1 << "  ";
    }
    std::cout << std::endl;
}

struct Move {
    int move_num;
    int src;
    int dest;
};

int main() {
    std::ifstream infile("d05.in");
    std::string curline;
    std::vector<Move> moves;
    std::vector<char> stacks[NUM_COLS];
    bool parse_stacks = true;
    while (getline(infile, curline)) {
        if (curline.size() == 0) {
            continue;
        }

        if (parse_stacks) {
            std::istringstream ss(curline);
            std::cout << curline << std::endl;
            int col = 0;
            char buf[4] = {0};
            while (!ss.eof()) {
                // read 4chars -> assign content to string s
                ss.read((char *)&buf, 4);
                // s.assign((char *)&buf, 4);
                if (buf[1] == '1') {
                    // line that shows stack numbers
                    parse_stacks = false;
                    // reverse vectors
                    for (int i = 0; i < NUM_COLS; ++i) {
                        std::reverse(stacks[i].begin(), stacks[i].end());
                    }
                    // std::cout << "Parse stacks done" << std::endl;
                    break;
                } else if (buf[0] != '[') {
                    // skip empty col
                    // std::cout << "Skipping col " << col << std::endl;
                    col += 1;
                    continue;
                }
                // std::cout << "S: " << buf << std::endl;
                char box = buf[1];
                stacks[col].push_back(box);
                // std::cout << "Stack " << +col << "[" << box << "]" << std::endl;
                col += 1;
            }
        } else {
            std::string s;
            std::istringstream ss(curline);
            getline(ss, s, ' ');
            getline(ss, s, ' ');
            int move_num = std::stoi(s);
            getline(ss, s, ' ');
            getline(ss, s, ' ');
            int src = std::stoi(s);
            getline(ss, s, ' ');
            getline(ss, s, '\n');
            int dest = std::stoi(s);
            // std::cout << "n=" << move_num << ": " << src << "->" << dest << std::endl;
            // std::cout << "src(" << stacks[src - 1].size() << ")" << std::endl;
            moves.push_back({ move_num, src, dest });
        }
    }

    std::cout << "^ should match v" << std::endl;
    print_stacks(stacks);

    {
        std::vector<char> stacks_copy[NUM_COLS];
        for (int i = 0; i < NUM_COLS; ++i) {
            // uses the copy constructor
            stacks_copy[i] = stacks[i];
        }

        for (const auto& move : moves) {
            // do the move
            // NOTE: moves -> ONE CRATE AT A TIME
            for (int i = 0; i < move.move_num; ++i) {
                // src and dest are 1-BASED
                char to_move = stacks_copy[move.src - 1].back();
                // std::cout << "moving " << to_move << " from " << src << " to " << dest << std::endl;
                stacks_copy[move.dest - 1].push_back(to_move);
                // actually remove it from src
                stacks_copy[move.src - 1].pop_back();
            }
        }
        std::cout << "Part1: Top crates '";
        for (int i = 0; i < NUM_COLS; ++i) {
            std::cout << stacks_copy[i].back();
        }
        std::cout << "'" << std::endl;
    }

    {
        std::vector<char> stacks_copy[NUM_COLS];
        for (int i = 0; i < NUM_COLS; ++i) {
            // uses the copy constructor
            stacks_copy[i] = stacks[i];
        }

        for (const auto& move : moves) {
            // do the move
            // NOTE: moves -> ALL CRATES AT ONCE
            auto& dest = stacks_copy[move.dest - 1];
            auto& src = stacks_copy[move.src - 1];
            // insert at dest.end() move_num items from the end of src
            dest.insert(dest.end(), src.end() - move.move_num, src.end());
            src.resize(src.size() - move.move_num);
        }
        std::cout << "Part2: Top crates '";
        for (int i = 0; i < NUM_COLS; ++i) {
            std::cout << stacks_copy[i].back();
        }
        std::cout << "'" << std::endl;
    }

}
