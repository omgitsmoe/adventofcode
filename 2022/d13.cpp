#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <variant>
#include <charconv>
#include <algorithm>

// try std::variant instead of the C way with using bare unions with a manual tag
// so constructors get handled etc. - requires c++17
// typedef alternative -> using
// cant do this:
// using Packet = std::variant<int, std::vector<Packet>>;
// since Packet doesn't exist yet
// can't forward 'declare' a using type alias since using isn't for new types
// so we have to use a struct that inherits from variant
// see https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p2162r0.html#inheriting-from-variant
struct Packet : std::variant<int, std::vector<Packet>> {
    // using std::vector<std::variant<int, Packet>>::vector;
    // so we can use the base class' constructor
    // see https://en.cppreference.com/w/cpp/language/using_declaration -> Inheriting constructors
    using variant::variant;
};
using Packets = std::vector<Packet>;

Packet parse_num(const std::string& from, size_t start, size_t& consumed_out) {
    size_t end = start;
    while (end < from.size() && from[end] >= '0' && from[end] <= '9') {
        end += 1;
    }
    // use std::from_chars which works for string_view/ptrs so we don't have to make a copy
    int num;
    auto result = std::from_chars(&from[start], &from[end], num);
    // check for errors
    if (result.ec == std::errc::invalid_argument) {
        std::cout << "That isn't a number.\n";
    } else if (result.ec == std::errc::result_out_of_range) {
        std::cout << "This number is larger than an int.\n";
    }

    // chars processed
    consumed_out = end - start;

    return Packet(num);
}

Packet parse_packet(const std::string& from, size_t start, size_t& consumed_out) {
    // std::cout << "[";
    std::vector<Packet> result;
    size_t idx = start;
    while (idx < from.size() && from[idx] != ']') {
        char c = from[idx];
        size_t advance = 1;
        if (c == '[') {
            // past [
            idx += 1;
            result.emplace_back(parse_packet(from, idx, advance));
        } else if (c >= '0' && c <= '9') {
            result.emplace_back(parse_num(from, idx, advance));
            // std::cout << std::get<int>(result[result.size() - 1]) << ", ";
        }

        idx += advance;
    }
    
    // +1 to consume ]
    consumed_out = (idx + 1) - start;

    // std::cout << "]";
    return Packet(result);
}

enum struct Ordering { in_order, partial_cmp, out_of_order };

// NOTE: they don't want you to compare the whole list item for item,
// if the first item is smaller or greater than the other then we stop there
// only in the case all items match we need to compare list sizes
// this is explained very badly in the task description
// since it litaterlly states:
// "If both values are lists, compare the first value of each list, then the second value, and so on."
// ok, the first part about only checking the next input if the ints are equal also applies
// to the lists...
Ordering compare_packets(const Packet& p1, const Packet& p2) {
    bool p1_is_int = std::holds_alternative<int>(p1);
    bool p2_is_int = std::holds_alternative<int>(p2);

    if (p1_is_int && p2_is_int) {
        // both ints
        // out of order if p1 greater; lower int should come first
        int int1 = std::get<int>(p1);
        int int2 = std::get<int>(p2);
        // std::cout << "Cmp " << int1 << " vs " << int2 << std::endl;
        if (int1 > int2) {
            return Ordering::out_of_order;
        } else if (int1 == int2) {
            return Ordering::partial_cmp;
        } else {
            return Ordering::in_order;
        }
    } else if (!p1_is_int && !p2_is_int) {
        // both lists
        const auto& list1 = std::get<Packets>(p1);
        const auto& list2 = std::get<Packets>(p2);
        int cmp_idx = 0;
        while (cmp_idx < list1.size() && cmp_idx < list2.size()) {
            auto cmp_result = compare_packets(list1[cmp_idx], list2[cmp_idx]);
            if (cmp_result != Ordering::partial_cmp) {
                // std::cout << "List cmp end " << (cmp_result == Ordering::in_order ? "inorder" : "outof") << std::endl;
                return cmp_result;
            }
            cmp_idx += 1;
        }

        // ran out of items
        // right less inputs -> out of order
        // otherwise continue
        if (list1.size() > list2.size()) {
            // std::cout << "List cmp end outof" << std::endl;
            return Ordering::out_of_order;
        } else if (list1.size() < list2.size()) {
            // left ran out of items first
            return Ordering::in_order;
        } else {
            // equal length -> check next input
            // std::cout << "List cmp end partial_cmp" << std::endl;
            // this is only partial_cmp since all elements were equal
            return Ordering::partial_cmp;
        }
    } else {
        // __exactly__ one value is an int
        // make a list out of the int and compare
        if (p1_is_int) {
            const auto p1_wrapped = Packets{std::get<int>(p1)};
            const auto& list2 = std::get<Packets>(p2);
            return compare_packets(p1_wrapped, list2);
        } else {
            const auto& list1 = std::get<Packets>(p1);
            const auto p2_wrapped = Packets{std::get<int>(p2)};
            return compare_packets(list1, p2_wrapped);
        }
    }
}

void print_packet(const Packet& p) {
    if (std::holds_alternative<int>(p)) {
        int unwrapped = std::get<int>(p);
        std::cout << unwrapped << ", ";
    } else {
        const auto& unwrapped = std::get<Packets>(p);
        std::cout << "[";
        for (const auto& p_inner : unwrapped) {
            print_packet(p_inner);
        }
        std::cout << "]";
    }
}

// needs /std:c++17
int main() {
    std::ifstream infile("d13.in");
    std::string line;
    Packets ps;

    while (getline(infile, line)) {
        if (line.size() == 0) continue;
        size_t adv;
        // start one past first [
        ps.emplace_back(parse_packet(line, 1, adv));
        // std::cout << std::endl;
    }

    size_t indices_sum = 0;
    for (int p_idx = 0; (p_idx + 1) < ps.size(); p_idx += 2) {
        auto& p1 = ps[p_idx];
        auto& p2 = ps[p_idx + 1];

        if (compare_packets(p1, p2) == Ordering::in_order) {
            // divied by 2 to get pair index + 1 since 1-based
            size_t pair_index = p_idx / 2 + 1;
            // std::cout << "Pair idx: " << pair_index << std::endl;
            indices_sum += pair_index;
        }
    }

    std::cout << "Part1: Ordered paird indices sum: " << indices_sum << std::endl;

    // add additional divider packets
    // WTF!?: for some reason this does not work
    // ps.emplace_back(Packets{ Packets{ Packet(2) } });
    // ps.emplace_back(Packets{ Packets{ Packet(6) } });
    // ^ just results in [6] instead of [[6]]
    auto div1 = Packets{};
    div1.emplace_back(Packets{ Packet(2) });
    auto div2 = Packets{};
    div2.emplace_back(Packets{ Packet(6) });
    ps.push_back(div1);
    ps.push_back(div2);
    // use sort with custom lambda for sorting which expects to return true if
    // lhs < rhs
    std::sort(ps.begin( ), ps.end( ), [](const auto& lhs, const auto& rhs)
    {
        auto cmp = compare_packets(lhs, rhs);
        return cmp == Ordering::in_order; // same as lhs < rhs
        // return lhs.key < rhs.key;
    });

    // for (const auto& p : ps) {
    //     print_packet(p);
    //     std::cout << std::endl;
    // }

    // find idx (1-based) of the divider packets and multiply them together to find:
    size_t decoder_key = 1;
    size_t packet_idx = 1;
    for (const auto& p : ps) {
        // is there a more concise way to do this?
        const auto& unwrapped = std::get<Packets>(p);
        if (unwrapped.size() == 1 && std::holds_alternative<Packets>(unwrapped.front())) {
            // std::cout << "first item packets " << packet_idx << std::endl;
            const auto& inner = std::get<Packets>(unwrapped.front());
            if (inner.size() == 1 && (inner.front() == Packet(2) || inner.front() == Packet(6))) {
                // std::cout << "found divider" << std::endl;
                decoder_key *= packet_idx;
            }
        }
        packet_idx += 1;
    }
    std::cout << "Part2: Decoder key: " << decoder_key << std::endl;
}
