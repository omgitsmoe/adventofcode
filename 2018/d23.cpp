#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <vector>
#include <algorithm>    // std::max
#include <queue>

using ll = long long;

struct Bot {
    ll x, y, z, r = 0;
    /* Bot () { x = 0; y = 0; z = 0; r = 0; }; */ 
    /* Bot (ll x1, ll y1, ll z1, ll r1) { x = x1; y = y1; z = z1; r = r1; }; */ 
    /* // copy constructor */
    /* Bot (const Bot &old) { x = old.x; y = old.y; z = old.z; r = old.r; }; */ 
};

/* inline ll abs(ll x) { */
/*     if (x < 0) { */
/*         return x * -1; */
/*     } */
/*     return x; */
/* } */

inline ll manhattan_dist(const Bot &b1, const Bot &b2) {
    return abs(b1.x - b2.x) + abs(b1.y - b2.y) + abs(b1.z - b2.z);
}

inline int is_in_range(const Bot &b1, const Bot &b2) {
        // considered in range if manhattan dist is equal or smaller than bot's radius
        if (manhattan_dist(b1, b2) <= b1.r) {
            return 1;
        } else {
            return 0;
        }
}

struct Box {
    ll x, y, z, size = 0;
    int in_range_of_bots = 0;
};

// define comparison operator for priority queue to work
inline bool operator <(const Box& x, const Box& y) {
    // biggest nr of bots in range > smallest dist to origin > smallest size
    // since this is less than comparison and STL priority queue sorts
    // bigger items first we need to reverse comparison on dist and size since
    // we want the smaller to sort first
    if (x.in_range_of_bots < y.in_range_of_bots) return true;
    else if (x.in_range_of_bots == y.in_range_of_bots) {
        if ((x.x + x.y + x.z) > (y.x + y.y + y.z)) return true;
        else if ((x.x + x.y + x.z) == (y.x + y.y + y.z)) {
            return (x.size > y.size);
        } else { return false; }
    } else { return false; }
}

inline ll clamp(ll val, ll lo, ll hi) {
    return val < lo ? lo : hi < val ? hi : val;
}

inline int box_in_range(const Box &box, const Bot &bot) {
    // from mdn 3d collision (https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection)
    // aabb vs. point
    /* return (point.x >= box.minX && point.x <= box.maxX) && */
    /*      (point.y >= box.minY && point.y <= box.maxY) && */
    /*      (point.z >= box.minZ && point.z <= box.maxZ); */
    /* return (bot.x >= box.x && bot.x <= (box.x + box.size)) && */
    /*      (bot.y >= box.y && bot.y <= (box.y + box.size)) && */
    /*      (bot.z >= box.z && bot.z <= (box.z + box.size)); */
    // above check if point/bot is inside box but we want to check if the cube
    // is in range of the nanobots radius
    // sphere vs. aabb
    // get box closest point to sphere center by clamping
    /* var x = Math.max(box.minX, Math.min(sphere.x, box.maxX)); */
    /* var y = Math.max(box.minY, Math.min(sphere.y, box.maxY)); */
    /* var z = Math.max(box.minZ, Math.min(sphere.z, box.maxZ)); */

    /* // this is the same as isPointInsideSphere */
    /* var distance = Math.sqrt((x - sphere.x) * (x - sphere.x) + */
    /*                        (y - sphere.y) * (y - sphere.y) + */
    /*                        (z - sphere.z) * (z - sphere.z)); */

    /* return distance < sphere.radius; */
    // clamping with min max
    // std::min( std::max( value, min_value ), max_value );
    // or other way around
    // get box's closest point to sphere center by clamping it to box's dimensions
    ll x = std::max(box.x, std::min(bot.x, box.x + box.size));
    ll y = std::max(box.y, std::min(bot.y, box.y + box.size));
    ll z = std::max(box.z, std::min(bot.z, box.z + box.size));
    /* ll x = clamp(bot.x, box.x, box.x + box.size); */
    /* ll y = clamp(bot.y, box.y, box.y + box.size); */
    /* ll z = clamp(bot.z, box.z, box.z + box.size); */
    // use manhattan dist here -> since we're using manhattan dist the nanobots with their radi
    // arent spheres but rather they're octahedrons
    ll dist = abs(x - bot.x) + abs(y - bot.y) + abs(z - bot.z);
    if (dist <= bot.r) {
        return 1;
    } else {
        return 0;
    }
}

int bots_in_range(const Box &box, std::vector<Bot> *bots) {
    int count = 0;
    for (int i = 0; i < bots->size(); ++i) {
        count += box_in_range(box, (*bots)[i]);
    }
    return count;
}


int main()
{
    std::ifstream infile("d23.in");
    std::vector<Bot> bots;
    //R"delim(…)delim" for raw string
    std::regex regex(R"_(pos=<([-0-9]+),([-0-9]+),([-0-9]+)>, r=(\d+))_");
    std::smatch matches;
    std::string line;
    Bot b;
    while (std::getline(infile, line, '\n'))
    {
        /* std::istringstream iss(line); */
        /* int a, b; */
        /* if (!(iss >> a >> b)) { break; } // error */
        if (std::regex_search(line, matches, regex))
        {
          // std::cout << "match " << matches.str(1) << " " matches.str(2) << " " << matches.str(3) << std::endl;
          // std::string date = matches[0];
          b.x = std::stoll(matches.str(1));
          b.y = std::stoll(matches.str(2));
          b.z = std::stoll(matches.str(3));
          b.r = std::stoll(matches.str(4));
          /* push_back() method will make a copy of that object that is now
           * owned by the vector. You need to make sure your objects can be
           * properly copied (copy-constructor and assignment operator may be
           * necessary). Copies contained in the vector in this example are
           * owned by the vector, and will have object lifetimes that are
           * similar to the vector itself */
          bots.push_back(b);
        } 
    }
    // find bot with max radius
    int max_rad_i = 0;
    for (int i = 0; i < bots.size(); ++i) {
        if (bots[i].r > bots[max_rad_i].r) max_rad_i = i;
    }
    /* std::cout << "x " << bots[max_rad_i].x << std::endl; */
    /* std::cout << "y " << bots[max_rad_i].y << std::endl; */
    /* std::cout << "z " << bots[max_rad_i].z << std::endl; */
    /* std::cout << "r " << bots[max_rad_i].r << std::endl; */
    int in_range = 0;
    for (int i = 0; i < bots.size(); ++i) {
        in_range += is_in_range(bots[max_rad_i], bots[i]);
    }
    std::cout << "Part1: " << in_range << std::endl;
    
    // part2
    /* std::cout << "Inside: " << box_in_range(Box{0, 0, 0, 100, 0}, Bot{10, 55, 99, 0}) << std::endl; */
    // std::cout << "Bots in range: " << bots_in_range(Box{0, 0, 0, 10000, 0}, &bots) << std::endl;
    
    // get bounds of point cloud
    ll min[3] = {100000, 100000, 100000};
    ll max[3] = {0, 0, 0};
    for (int i = 0; i < bots.size(); ++i) {
        if (bots[i].x > max[0]) max[0] = bots[i].x;
        if (bots[i].y > max[1]) max[1] = bots[i].y;
        if (bots[i].z > max[2]) max[2] = bots[i].z;
        if (bots[i].x < min[0]) min[0] = bots[i].x;
        if (bots[i].y < min[1]) min[1] = bots[i].y;
        if (bots[i].z < min[2]) min[2] = bots[i].z;
    }

    // presumably correct:
    /* Nearest the big bot: 652 */
    /* The max count I found was: 978 */
    /* Best location value: 164960498 */
    std::priority_queue<Box> queue;
    // create box which starts at min xyz and has a size of the max diff
    // between the min and max of any of the axes
    // just use size instead of using maxxyz since its a cube and the sizes match on all sides
    // size 4 means that if we start at minx 0 maxx will be at 4
    queue.push(Box{min[0], min[1], min[2],
                   std::max(std::abs(min[0] - max[0]),
                            std::max(std::abs(min[1] - max[1]),
                                     std::abs(min[2] - max[2]))),
                            1000});
    
    // test if we acutally have 1000 bots in range
    std::cout << bots_in_range(queue.top(), &bots) << std::endl;
    std::cout << queue.top().size << std::endl;
    while (!queue.empty()) {
        // assign item with top priority (biggest nr of bots in range)
        // and pop it from queue
        auto current = queue.top();
        queue.pop();
        /* std::cout << "Curr: " << current.in_range_of_bots << " Size " << current.size << std::endl; */

        // std::cout << "Curr: " << current.x + current.y + current.z << " with " << current.in_range_of_bots << " bots in range at size " << current.size << " xyz: " << current.x << ", " << current.y << ", " << current.z << std::endl;
        if (current.size == 0) {
            // box is just a point now
            std::cout << "Part2: " << current.x + current.y + current.z << " with " << current.in_range_of_bots << " bots in range" << std::endl;
            break;
        }
        // divide box in 8 sub-boxes with halfed size
        ll new_size = current.size / 2; 
        // keep sub-cube's size even so we dont miss points when we divide it the next time
        // obv. not possible when we reach size 1
        if (new_size > 1) new_size += new_size % 2;
        Box sub1 = {current.x, current.y, current.z, new_size};
        sub1.in_range_of_bots = bots_in_range(sub1, &bots);
        queue.push(sub1);
        Box sub2 = {current.x + new_size, current.y, current.z, new_size};
        sub2.in_range_of_bots = bots_in_range(sub2, &bots);
        queue.push(sub2);
        Box sub3 = {current.x, current.y + new_size, current.z, new_size};
        sub3.in_range_of_bots = bots_in_range(sub3, &bots);
        queue.push(sub3);
        Box sub4 = {current.x + new_size, current.y + new_size, current.z, new_size};
        sub4.in_range_of_bots = bots_in_range(sub4, &bots);
        queue.push(sub4);
        Box sub5 = {current.x, current.y, current.z + new_size, new_size};
        sub5.in_range_of_bots = bots_in_range(sub5, &bots);
        queue.push(sub5);
        Box sub6 = {current.x + new_size, current.y, current.z + new_size, new_size};
        sub6.in_range_of_bots = bots_in_range(sub6, &bots);
        queue.push(sub6);
        Box sub7 = {current.x, current.y + new_size, current.z + new_size, new_size};
        sub7.in_range_of_bots = bots_in_range(sub7, &bots);
        queue.push(sub7);
        Box sub8 = {current.x + new_size, current.y + new_size, current.z + new_size, new_size};
        sub8.in_range_of_bots = bots_in_range(sub8, &bots);
        queue.push(sub8);
    }
    return 0;
}
