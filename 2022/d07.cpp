#include <iostream>
#include <fstream>
#include <string>
#include <vector>

struct Dir {
    std::string name;
    size_t size;
};

int main() {
    std::ifstream infile("d07.in");
    std::string line;
    std::vector<Dir> stack;
    std::vector<Dir> finished;
    // NOTE: assumes the cd/ls are in DFS order
    while (getline(infile, line)) {
        if (line.size() == 0) continue;
        // limit rfind to only match at pos 0 or earlier -> to get a startswith
        // std::cout << line << std::endl;
        if (line.rfind("$ cd", 0) == 0) {
            // assumes there are no dirs with a dot prefix
            if (line[5] == '.') {
                Dir finished_dir = stack.back();
                stack.pop_back();
                finished.push_back(finished_dir);
                // std::cout << "Finished " << finished_dir.name << " s: " << finished_dir.size << std::endl;
                // add dir size to parent
                if (stack.size() > 0) {
                    stack.back().size += finished_dir.size;
                }
            } else {
                // add new dir to stack
                const int dirname_end = line.find(" ", 5);
                stack.push_back(Dir{ line.substr(5, dirname_end - 5), 0 });
                // std::cout << "cd " << stack.back().name << std::endl;
            }
        } else if ((line[0] >= '0') && (line[0] <= '9')) {
            // file with size
            const int size_end = line.find(" ");
            size_t file_size = std::stoi(line.substr(0, size_end));
            stack.back().size += file_size;
            // std::cout << "file " << file_size << "b" << std::endl;
        }
    }

    while (stack.size() > 0) {
        Dir finished_dir = stack.back();
        stack.pop_back();
        finished.push_back(finished_dir);
        // std::cout << "Finished " << finished_dir.name << " s: " << finished_dir.size << std::endl;
        // add dir size to parent
        if (stack.size() > 0) {
            stack.back().size += finished_dir.size;
        }
    }

    constexpr int MAX_DIR_SIZE = 100000;
    size_t sum = 0;
    for (const auto& d : finished) {
        if (d.size <= MAX_DIR_SIZE) {
            sum += d.size;
        }
    }

    std::cout << "Part1: " << sum << std::endl;

    constexpr int FS_SIZE = 70000000;
    constexpr int NEEDED_FOR_UPDATE = 30000000;
    // fs size - root size -> available space
    const int current_free = FS_SIZE - finished.back().size;
    const int needed = NEEDED_FOR_UPDATE - current_free;

    Dir &smallest = finished.back();
    for (const auto& d : finished) {
        if (d.size >= needed && d.size < smallest.size) {
            smallest = d;
        }
    }

    std::cout << "Part2: Total size of smallest dir (" << smallest.name
        << ") to delete " << smallest.size << std::endl;
}
