#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

class SplitIter {
    std::string& m_in;
    std::string m_pattern;
    // end index of last returnd part + pattern
    // a, b, c
    // m_last_end = 0
    // return a
    // m_last_end = 3; (idx of , -> 1 + pattern length 2 => 3)
    size_t m_last_end = 0;
public:
    SplitIter(std::string& in, std::string pattern, size_t start_idx = 0)
        : m_in(in), m_pattern(std::move(pattern)), m_last_end(start_idx) {}

    bool more() {
        return m_last_end < m_in.size();
    }

    void next(std::string& out) {
        if (m_last_end >= m_in.size()) {
            // reached end of string
            return;
        }

        size_t next_patt_idx = m_in.find(m_pattern, m_last_end);
        if (next_patt_idx == std::string::npos) {
            // till end of string
            out = m_in.substr(m_last_end);
            m_last_end = m_in.size();
        } else {
            out = m_in.substr(m_last_end, next_patt_idx - m_last_end);
            m_last_end = next_patt_idx + m_pattern.size();
        }
    }

    std::vector<std::string> collect() {
        std::vector<std::string> result;
        std::string part;
        while (this->more()) {
            this->next(part);
            result.push_back(part);
        }

        return result;
    }

    SplitIter& operator=(const SplitIter& rhs) {
        // we're being assigned the 'contents' of rhs
        m_in = rhs.m_in;
        m_pattern = std::move(rhs.m_pattern);
        m_last_end = rhs.m_last_end;

        // return ref to self
        return *this;
    }
};

// pattern: substr that separates last "word" (usually a space)
int last_word_to_num(const std::string& in, std::string&& pattern) {
    size_t space_before_num_idx = in.rfind(pattern);
    return std::stoi(in.substr(space_before_num_idx + 1));
}

// need long long (at least 64bit; long at least 32bit; int at least 16bit) so
// the math doesn't overflow, esp. squaring
long long do_op(char op, long long lhs, long long rhs, bool op_with_self) {
    if (op_with_self) {
        rhs = lhs;
    }

    switch (op) {
        case '+':
            return lhs + rhs;
        case '-':
            return lhs - rhs;
        case '*':
            return lhs * rhs;
        case '/':
        default:
            std::cout << "Unexpected operator" << std::endl;
            return -1;
    }
}

struct Monkey {
    std::vector<long long> items;

    char op;
    bool op_with_self;
    int operand_rhs;

    int test;
    int if_true;
    int if_false;
};

void sim_round(std::vector<Monkey>& monkeys, std::vector<size_t>& inspected, long long scm_mod) {
    // monkeys take turns
    for (size_t i = 0; i < monkeys.size(); ++i) {
        Monkey& m = monkeys[i];

        // each inspecting all the items they're holding
        for (auto& worry : m.items) {
            // inspect item and apply worry change
            // std::cout << "op " << m.op << " worry " << worry << " rhs " << m.operand_rhs << " sq " << m.op_with_self << std::endl;
            worry = do_op(m.op, worry, m.operand_rhs, m.op_with_self);

            // before testing the worry is divided by 3 and rounded to nearest int
            // deactivated for part2: worry = worry / 3;

            // for part2
            // numbers are growing too quickly so we have to reduce them while still keeping
            // the same divisibility:
            // u/1234abcdcba4321: For any integer n which is divisible by P, n-kP is also
            // divisible by P (and same for if it's not).
            // (Also, if n+S is divisible by P, so is (n-kP)+S.)
            // (-> picking a lower multiple, k is arbitrary)
            // => doing n-kP is the same as using modulo -> divisible = 0 (which can be n-kP)
            //    -> otherwise = remainder of n/P
            //    (n % P equivalent to: repeated subtraction of P until n < P or n==0)
            // this will keep the divisibility for one P
            // but we need to keep divisibility for multiple P
            // what keeps all the possible remainders for multiple P?
            // -> a multiple of all those different P -> use the smallest common multiple
            //    e.g. if divsible by 2 and 3, so is n-6k
            // => smallest common multiple
            // -> all divisibility tests here use primes -> scm is just the product of the primes
            // (non-primes could have a lower common multiple)
            // we basically use below, where k is the product of our primes
            // x mod n = (x mod kn) mod n
            worry = worry % scm_mod;

            // test and throw item
            if ((worry % ((long long)m.test)) == 0) {
                // if_true
                // m.test is a prime
                monkeys[m.if_true].items.push_back(worry);
            } else {
                // if_false
                monkeys[m.if_false].items.push_back(worry);
            }

            // inc inspection times
            inspected[i] += 1;
        }

        // assume monkey can't throw an item to itself, so we can just clear the vector
        m.items.clear();
    }
}

int main() {
    std::ifstream infile("d11.in");
    std::string line;
    std::vector<Monkey> monkeys;
    while (true) {
        // assume monkeys can be parsed sequentially
        // get and discard Monkey X: line
        if (!getline(infile, line)) {
            std::cout << "Out of input!" << std::endl;
            break;
        }
        Monkey m;
        // items
        getline(infile, line);
        // std::cout << "Line: " << line << std::endl;
        auto iter = SplitIter(line, ", ", 18);
        std::string part;
        while (iter.more()) {
            iter.next(part);
            m.items.push_back(std::stoi(part));
        }
        // std::cout << "Items: ";
        // for (auto i : m.items) {
        //     std::cout << i << ", ";
        // }
        // std::cout << std::endl;

        // operation
        // new = old * operand
        //           ^ 23
        getline(infile, line);
        // uses assignment operator
        iter = SplitIter(line, " ", 23);
        iter.next(part);
        m.op = part[0];
        iter.next(part);
        if (part[0] == 'o') {
            m.op_with_self = true;
        } else {
            m.op_with_self = false;
            m.operand_rhs = std::stoi(part);
        }
        // std::cout << "op " << m.op << " self " << m.op_with_self << " rhs " << m.operand_rhs << std::endl;

        // test
        getline(infile, line);
        m.test = last_word_to_num(line, " ");
        // std::cout << "test mod " << m.test << std::endl;

        // if true
        getline(infile, line);
        m.if_true = last_word_to_num(line, " ");
        // std::cout << "if_true  " << m.if_true << std::endl;

        // if false
        getline(infile, line);
        m.if_false = last_word_to_num(line, " ");
        // std::cout << "if_false  " << m.if_false << std::endl;

        monkeys.push_back(m);

        // consume empty line
        getline(infile, line);
    }

    // for (const auto& m : monkeys) {
    //     std::cout << "Monkey: Test mod " << m.test << std::endl;
    // }

    {
        // copy vec
        auto monkeys_cp = monkeys;
        // times a monkey inspected an item
        std::vector<int> inspected(monkeys_cp.size(), 0);
        size_t round = 0;
        while (round < 20) {
            // monkeys_cp take turns
            for (size_t i = 0; i < monkeys_cp.size(); ++i) {
                Monkey& m = monkeys_cp[i];

                // each inspecting all the items they're holding
                for (auto& worry : m.items) {
                    // inspect item and apply worry change
                    worry = do_op(m.op, worry, m.operand_rhs, m.op_with_self);
                    // before testing the worry is divided by 3 and rounded to nearest int
                    worry = worry / 3;
                    // test and throw item
                    if ((worry % m.test) == 0) {
                        // if_true
                        monkeys_cp[m.if_true].items.push_back(worry);
                    } else {
                        // if_false
                        monkeys_cp[m.if_false].items.push_back(worry);
                    }

                    // inc inspection times
                    inspected[i] += 1;
                }

                // assume monkey can't throw an item to itself, so we can just clear the vector
                m.items.clear();
            }
            round += 1;
        }

        for (auto i : inspected) {
            std::cout << "inspected " << i << std::endl;
        }

        // sort descending
        std::sort(inspected.rbegin(), inspected.rend());
        std::cout << "Part1: Level of monkey business " << inspected[0] * inspected[1] << std::endl;
    }

    {
        // copy vec
        auto monkeys_cp = monkeys;
        // times a monkey inspected an item
        std::vector<size_t> inspected(monkeys_cp.size(), 0);
        // find lowest common multiple of the divisibility tests, so we can use it to
        // keep the numbers small
        int lowest_common_multiple = 1;
        for (const auto& m : monkeys) {
            lowest_common_multiple *= m.test;
        }
        std::cout << "Lowest common multiple " << lowest_common_multiple << std::endl;

        size_t round = 0;
        while (round < 10000) {
            sim_round(monkeys_cp, inspected, lowest_common_multiple);
            round += 1;
        }

        std::sort(inspected.rbegin(), inspected.rend());
        std::cout << "Part2: Level of monkey business " << inspected[0] * inspected[1] << std::endl;
        std::cout << "Part2: Level of monkey business " << inspected[0] << "*" << inspected[1] << std::endl;
    }
}
