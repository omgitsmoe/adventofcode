#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::ifstream infile("d10.in");
    std::string line;
    int cycle = 1;
    int reg = 1;
    int wait_cycles = 0;
    int val = 0;
    int sum_signal_strengths = 0;
    char crt[6][40];
    while (cycle <= 240) { // 240 for part2 (instead of 220 for part1)
        // so we don't miss cycles when checking for 20, 40 etc. (part1)
        while (wait_cycles > 0) {
            if (((cycle + 20) % 40) == 0) {
                std::cout << "Cycle " << cycle << " reg " << reg << std::endl;
                sum_signal_strengths += reg * cycle;
            }
            // part2
            // draw the crt pixel and check if sprite (width 3) visible (reg is middle pixel)
            // 1px per cycle so we can calculate the y/x coords from it
            int crt_x = (cycle - 1) % 40;
            int crt_y = (cycle - 1) / 40;
            // sprite may be 1px off to the side to be still drawn
            if (std::abs(crt_x - reg) <= 1) {
                crt[crt_y][crt_x] = '#';
            } else {
                crt[crt_y][crt_x] = '.';
            }

            wait_cycles -= 1;
            cycle += 1;


            if (wait_cycles == 0) {
                // std::cout << "OP cycle " << cycle << " + " << val << std::endl;
                // apply addx instruction
                reg += val;
                // reset
                val = 0;
            }
        }

        // get new instruction
        if (!getline(infile, line)) {
            std::cout << "no more input" << std::endl;
            break;
        }
        if (line.size() == 0) {
            continue;
        } else if (line[0] == 'n') {
            // noop 1 cycle
            wait_cycles += 1;
        } else if (line[0] == 'a') {
            // addx -> AFTER 2 cycles -> reg+val
            val = std::stoi(line.substr(5));
            wait_cycles += 2;
        }
    }

    std::cout << "Part1: Sum of signal strengths: " << sum_signal_strengths << std::endl;

    for (int y = 0; y < 6; ++y) {
        for (int x = 0; x < 40; ++x) {
            std::cout << crt[y][x];
        }
        std::cout << std::endl;
    }
}
