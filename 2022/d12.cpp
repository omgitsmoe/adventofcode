#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <deque>

#define INDEX(y, x) (((y) * cols) + (x))

struct Point {
    int y, x;
};

struct Node {
    char height;
    int path_cost;
    bool visited;
    Point pos;
    Point prev;
};

// pass-by-value supports both move and copy
// here we only want copies though
int dijkstra(std::vector<Node> grid, Point start, Point end, int cols) {
    int rows = grid.size() / cols;
    // Dijkstra algorithm for finding the shortest path
    // start node cost with cost 0
    auto& start_node = grid[INDEX(start.y, start.x)];
    start_node.path_cost = 0;
    start_node.visited = true;
    // should store sth. else, too much pointer chasing like this
    std::deque<Node*> q{ &start_node };

    // up, down, right, left
    Point neighbours[4] = { {1, 0}, {-1, 0}, {0, 1}, {0, -1} };
    while (!q.empty()) {
        // find queued node with min dist
        auto min = q.begin();
        for (auto it = q.begin(); it != q.end(); ++it) {
            // it = Node** => we have to deref once and then use ->
            if ((*it)->path_cost < (*min)->path_cost) {
                min = it;
            }
        }
        Node& current = **min;
        // actually remove the item
        q.erase(min);
        // std::cout << "P y " << current.y << " x " << current.x << std::endl;

        // visit the neighbours and set their costs/prev node, when the path we're currently on
        // is cheaper
        // + queue unvisited nodes
        for (auto n : neighbours) {
            Point next = Point{ current.pos.y + n.y, current.pos.x + n.x };
            if ((next.y < 0) || (next.y >= rows) || (next.x < 0) || (next.x >= cols)) {
                // out of range
                continue;
            }

            Node& next_node = grid[INDEX(next.y, next.x)];
            if ((next_node.height - current.height) > 1) {
                // elevation too high
                continue;
            }

            if (next_node.path_cost > (current.path_cost + 1)) {
                // set cheaper path cost
                next_node.path_cost = current.path_cost + 1;
                // update new path to the node
                next_node.prev = current.pos;
            }
            if (!next_node.visited) {
                // not yet visited
                // queue to visit adjacent nodes
                q.push_back(&next_node);
                next_node.visited = true;
            }
        }
    }

    // when all (reachable) nodes were visited
    // each node should have the lowest possible path (from start) cost set
    Point cur = end;
    int lowest_steps = grid[INDEX(cur.y, cur.x)].path_cost;

    // for finding the path
    // int steps = 0;
    // while (!((cur.y == start.y) && (cur.x == start.x))) {
    //     const Node& node = grid[INDEX(cur.y, cur.x)];
    //     std::cout << "(" << cur.y << ", " << cur.x << ")[" << node.height << "]" << std::endl;
    //     cur = node.prev;
    //     steps += 1;
    // }

    return lowest_steps;
}

int main() {
    std::ifstream infile("d12.in");
    std::string line;
    std::vector<Node> grid;
    Point start;
    Point end;
    int y = 0;
    int cols = -1;
    while (getline(infile, line)) {
        if (line.size() == 0) continue;

        if (cols < 0)
            cols = line.size();


        int x = 0;
        for (int x = 0; x < line.size(); ++x) {
            char height = line[x];
            if (height == 'S') {
                start.y = y;
                start.x = x;
                // give start proper height
                height = 'a';
            } else if (height == 'E') {
                end.y = y;
                end.x = x;
                // give end proper height
                height = 'z';
            }

            // set starting path_cost to INF/INT_MAX
            grid.emplace_back(Node{ height, INT_MAX, false, { y, x }, { -1, -1 } });
        }
        y += 1;
    }

    int rows = grid.size() / cols;


    int part1_steps = dijkstra(grid, start, end, cols);
    std::cout << "Part1: Lowest steps required " << part1_steps << std::endl;


    int part2_steps = INT_MAX;
    for (const auto& n : grid) {
        if (n.height == 'a') {
            const int steps = dijkstra(grid, n.pos, end, cols);
            if (steps < part2_steps) {
                part2_steps = steps;
            }
        }
    }
    std::cout << "Part2: Lowest steps required " << part2_steps << std::endl;
}
