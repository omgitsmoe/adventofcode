#include <iostream>
#include <fstream>
#include <string>

void find_unique_window(const std::string& in, int window_size = 4) {
    // so we have a window_size-sized window to start
    int idx = window_size - 1;
    while (idx < in.size() - 4) {
        // const char *window_start = idx - 3;
        // looking backwards
        std::string window = in.substr(idx - (window_size - 1), window_size);
        // std::cout << window << std::endl;
        bool unique = true;
        for (int i = 0; unique && (i < window.size()); ++i) {
            for (int j = i + 1; unique && (j < window.size()); ++j) {
                if (window[i] == window[j]) {
                    unique = false;
                }
            }
        }
        // cmps: n-1 + n-2 + .. + 1
        // roughly (n(n-1)) / 2
        // 1window -> 0
        // 2window -> 1
        // 3window -> 3
        // 4window -> 5cmp
        // 5window -> 10cmp
        // 6window -> 15cmp
        // 10window -> 45
        // 20window -> 190
        // 40windwo -> 780
        // => ~ O(n^2)

        if (unique) {
            // idx is at the __end__ of our window so idx + 1 is enough
            // otherwise idx + window_size
            std::cout << "Marker pos " << idx + 1 << std::endl;
            break;
        }
        idx += 1;
    }
}

int main() {
    std::ifstream infile("d06.in");
    std::string line;
    // we only have one line
    getline(infile, line);
    find_unique_window(line);
    find_unique_window(line, 14);
}
