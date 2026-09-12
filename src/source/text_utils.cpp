#include "loxmocha/source/text_utils.hpp"

#include <cstddef>
#include <string>
#include <string_view>

namespace loxmocha::source {

namespace {

    [[nodiscard]] constexpr auto is_horizontal_space(char character) -> bool
    {
        constexpr std::string_view horizontal_space = " \t\r";
        return horizontal_space.find(character) != std::string_view::npos;
    }

} // namespace

auto trim(std::string_view line) -> std::string_view
{
    std::size_t begin = 0;
    while (is_horizontal_space(line[begin])) {
        ++begin;
    }

    std::size_t end = line.size();
    while (is_horizontal_space(line[end - 1])) {
        --end;
    }

    return line.substr(begin, end - begin);
}

auto caret_marker(std::size_t column) -> std::string_view
{
    std::string marker(column - 1, ' ');
    marker.push_back('^');
    return marker;
}

auto prefix(std::string_view line, std::size_t count) -> std::string_view { return line.substr(0, count + 1); }

} // namespace loxmocha::source
