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
    constexpr std::string_view horizontal_space = " \t\r";

    const std::size_t begin = line.find_first_not_of(horizontal_space);
    if (begin == std::string_view::npos) {
        return {};
    }

    const std::size_t end = line.find_last_not_of(horizontal_space);
    return line.substr(begin, end - begin + 1);
}

auto caret_marker(std::size_t column) -> std::string
{
    const std::size_t padding = column > 0 ? column - 1 : 0;

    std::string marker(padding, ' ');
    marker.push_back('^');
    return marker;
}

auto prefix(std::string_view line, std::size_t count) -> std::string_view { return line.substr(0, count + 1); }

} // namespace loxmocha::source
