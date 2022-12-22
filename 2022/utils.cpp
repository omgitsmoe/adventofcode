#include <string>
#include <vector>
#include <string_view>

struct RangeHelper {
    size_t start;
    size_t end_exclusive;
};

class SplitIter {
    // TODO: also change this to a view?
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

    // NOTE: you will have to account for SplitIter always skipping
    // the pattern after the last returned part (see below)
    void change_pattern(std::string new_pattern) {
        // NOTE:
        // old ", " -> change_pattern(" ")
        // a, b, c d e f
        //    ^  ^
        //    |  | last_end/next returned part
        //    last returned part
        // => will work
        // old "," -> change_pattern(", ")
        // a,b, c, d, e, f
        //   ^ ^
        //   | | last_end/next returned part
        //   last returned part
        // => will return " c"
        m_pattern = std::move(new_pattern);
    }

    void skip() {
        advance();
    }

    void next(std::string& out) {
        RangeHelper next_part = advance();
        out = m_in.substr(next_part.start, next_part.end_exclusive - next_part.start);
    }

    void next_view(std::string_view& out) {
        RangeHelper next_part = advance();
        out = std::string_view(&m_in[next_part.start], next_part.end_exclusive - next_part.start);
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

private:
    RangeHelper advance() {
        RangeHelper result;
        if (m_last_end >= m_in.size()) {
            // reached end of string
            return { m_last_end, m_last_end };
        }

        size_t next_patt_idx = m_in.find(m_pattern, m_last_end);
        if (next_patt_idx == std::string::npos) {
            // till end of string
            result.start = m_last_end;
            result.end_exclusive = m_in.size();
            m_last_end = m_in.size();
        } else {
            result.start = m_last_end;
            result.end_exclusive = m_last_end + (next_patt_idx - m_last_end);
            m_last_end = next_patt_idx + m_pattern.size();
        }

        return result;
    }
    
};

