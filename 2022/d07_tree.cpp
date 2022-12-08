#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <tuple>

// in struct so we can access it with FSNode::file
// >=C++11 also with FSNode::Kind::file
// but they would also be accessible with FSNode x; x.file which would clash with the union
enum Kind { kind_file, kind_dir };
class FSNode {
public:
    Kind kind;
    FSNode *parent = nullptr;
    // std::variant should be used if a union contains non-trivial
    // (non-pod/non-auto-generated constructor) types
    // not needed, but wanted the practice
    union {
        struct {
            std::string name;
            size_t size;
        } file;
        struct {
            std::string name;
            size_t size;
            // need to use ptrs since ptrs to FSNode in the vec might get invalidated
            std::vector<FSNode*> children;
        } dir;
    };

    // && -> rvalue reference (rvalue has no memory address, e.g. result of expression not saved to a var)
    // if it has a name it's an lvalue -> &&name is an lvalue here so we need to
    // use std::move explicitly
    // -> but still better to just pass by value and then move
    FSNode(Kind in_kind, std::string name, size_t size, FSNode* p) : kind(in_kind), parent(p) {
        // NOTE: std::string name, passing a string literal (const char *) does not work
        // and will result in nullptr deref when trying to move it
        switch (in_kind) {
            case kind_file:
                // move string into the member var
                new(&file.name) std::string(std::move(name));
                file.size = size;
                break;
            case kind_dir:
                // since the union contains non-trivial (non-default constructor) types
                // they don't get initalized, so we have to initialize them inside the
                // union (in the sense of the memory address)
                // use placement new to init/construct at an already allocated address
                // (saying construct std::string at the address of dir.name)
                // (moving name into it)
                new(&dir.name) std::string(std::move(name));
                dir.size = size;
                new(&dir.children) std::vector<FSNode*>();

                break;
        }
    }

    // copy constructor
    // FSNode(const FSNode&) = default;
    // move constructor
    FSNode(FSNode &&in) {
        switch (kind) {
            case kind_file:
                // move the rvalue string into the member var
                file.name = std::move(in.file.name);
                file.size = in.file.size;
                break;
            case kind_dir:
                // move the rvalue string into the member var
                dir.name = std::move(in.dir.name);
                dir.size = in.file.size;
                dir.children = std::move(in.dir.children);

                break;
        }
    }

    ~FSNode() {
        // union member will non-trivial destructors must be provided
        // otherwise they won't be called
        switch (kind) {
            case kind_file:
                // std::string is a typedef for std::basic_string, standard specifies
                // that calling ~string should work (it works for custom types)
                // but it doesn't so we have to call ~basic_string
                file.name.~basic_string();
                break;
            case kind_dir:
                dir.name.~basic_string();
                // delete children from ptrs
                // NOTE: if the destructor gets called after a move/copy while we'll
                // delete the children while another instance is still holding on to them
                // -> use unique_ptr/shared
                for (auto *v : dir.children) {
                    delete v;
                }
                dir.children.~vector();
                break;
        }
    }
};

int main() {
    std::ifstream infile("d07.in");
    std::string line;

    FSNode root(kind_dir, "/", 0, nullptr);
    FSNode* current_dir = &root;
    while (getline(infile, line)) {
        if (line.size() == 0) continue;
        // limit rfind to only match at pos 0 or earlier -> to get a startswith
        // std::cout << line << std::endl;
        if (line.rfind("$ cd", 0) == 0) {
            // assumes there are no dirs with a dot prefix
            if (line[5] == '.') {
                current_dir = current_dir->parent;
            } else {
                // add new dir to stack
                const int dirname_end = line.find(" ", 5);
                FSNode *new_dir = new FSNode(kind_dir, line.substr(5, dirname_end - 5), 0, current_dir);
                current_dir->dir.children.push_back(new_dir);
                // set new current_dir
                current_dir = current_dir->dir.children.back();
            }
        } else if ((line[0] >= '0') && (line[0] <= '9')) {
            // file with size
            const int size_end = line.find(" ");
            size_t file_size = std::stoi(line.substr(0, size_end));
            FSNode *new_file = new FSNode(kind_file, "", file_size, current_dir);
            current_dir->dir.children.push_back(new_file);
            // std::cout << "file " << file_size << "b" << std::endl;
        }
    }

    // DFS traversal
    // could do this when building the tree, but this is better practice
    std::vector<std::tuple<bool, FSNode*>> stack{ std::make_tuple(false, &root) };
    std::vector<size_t> dir_sizes;
    while (stack.size() > 0) {
        bool finished;
        FSNode *current;
        // unpack tuple
        std::tie(finished, current) = stack.back();
        stack.pop_back();

        // when all subnodes of a dir were visited -> add size to parent
        if (finished) {
            // root has no parent
            if (current->parent) {
                current->parent->dir.size += current->dir.size;
            }
            dir_sizes.push_back(current->dir.size);
            continue;
        }


        switch (current->kind) {
            case kind_file:
                current->parent->dir.size += current->file.size;
                break;
            case kind_dir:
                // first queued will be last visited
                // queue dir again, this time as finished
                if (current->dir.children.size() > 0) {
                    stack.push_back(std::make_tuple(true, current));
                }
                for (auto n : current->dir.children) {
                    stack.push_back(std::make_tuple(false, n));
                }
                break;
        }
    }

    constexpr int MAX_DIR_SIZE = 100000;
    size_t sum = 0;
    for (auto size : dir_sizes) {
        if (size <= MAX_DIR_SIZE) {
            sum += size;
        }
    }

    std::cout << "Part1: " << sum << std::endl;

    constexpr int FS_SIZE = 70000000;
    constexpr int NEEDED_FOR_UPDATE = 30000000;
    // fs size - root size -> available space
    const int current_free = FS_SIZE - root.dir.size;
    const int needed = NEEDED_FOR_UPDATE - current_free;

    std::cout << needed << std::endl;
    // pick root_size to start
    size_t smallest_to_free = dir_sizes.back();
    for (auto size : dir_sizes) {
        if ((size >= needed) && (size < smallest_to_free)) {
            smallest_to_free = size;
        }
    }

    std::cout << "Part2: Total size of smallest dir to delete " << smallest_to_free << std::endl;
}
