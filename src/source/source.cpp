#include "loxmocha/source/source.hpp"

#include "loxmocha/memory/safe_pointer.hpp"

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace loxmocha::source {

namespace {
    /**
     * @brief Splits the given content into lines based on newline characters.
     * @param content The content to split into lines.
     * @return std::vector<std::string_view> A vector of string views, each representing a line in the content.
     */
    auto make_lines(std::string_view content) -> std::vector<std::string_view>
    {
        std::vector<std::string_view> lines{};
        const auto*                   line_start = content.begin();
        for (const auto* iter = content.begin(); iter != content.end(); std::advance(iter, 1)) {
            if (*iter == '\n') {
                lines.emplace_back(line_start, iter);
                line_start = std::next(iter);
            }
        }
        if (line_start != content.end()) {
            lines.emplace_back(line_start, content.end());
        }

        return lines;
    }
} // namespace

source_info_t::source_info_t(std::filesystem::path filepath, std::string&& content)
    : filepath_(std::move(filepath)), content_(std::move(content)), lines_(make_lines(content_))
{
}

auto source_info_t::find_location(std::string_view::iterator pos) const -> std::optional<source_location_t>
{
    return source_view_t{filepath_, content_, lines_}.find_location(pos);
}

auto source_info_t::find_span_location(std::string_view source_span) const
    -> std::optional<std::pair<source_location_t, source_location_t>>
{
    return source_view_t{filepath_, content_, lines_}.find_span_location(source_span);
}

auto source_view_t::find_location(std::string_view::iterator pos) const -> std::optional<source_location_t>
{
    // Find the the first line start position that is before input position.
    auto line = std::ranges::lower_bound(
        lines_.begin(), lines_.end(), pos, [](const std::string_view& line, const std::string_view& span) -> bool {
            // Here we true if the line is before the span
            // cppcheck-suppress mismatchingContainerExpression
            return line.end() <= span.begin();
        });

    // If we do not have lower bound then the position is not in the source.
    if (line == lines_.end()) {
        return std::nullopt;
    }

    // Otherwise we have found the line and return the location.
    return source_location_t{
        .line_span = *line,
        .line      = static_cast<std::size_t>(std::distance(lines_.begin(), line) + 1),
        .column    = static_cast<std::size_t>(pos - line->data() + 1),
    };
}

auto source_view_t::find_span_location(std::string_view source_span) const
    -> std::optional<std::pair<source_location_t, source_location_t>>
{
    auto start_loc = find_location(source_span.begin());
    if (!start_loc) {
        return std::nullopt;
    }

    auto end_loc = find_location(std::prev(source_span.end()));

    if (!end_loc) {
        return std::nullopt;
    }

    // The pair of locations is only returned if both locations are found.
    return std::make_pair(*start_loc, *end_loc);
}

auto source_manager_t::emplace(safe_ptr<source_info_t>&& source_info) -> std::pair<iterator_t, bool>
{
    auto result = filepath_map_.emplace(source_info->filepath(), std::move(source_info));

    if (result.second) {
        source_map_.emplace(result.first->second->content(), *(result.first->second));
    }

    return {iterator_t{result.first}, result.second};
}

auto source_manager_t::find_source(const std::filesystem::path& filepath) const -> source_manager_t::iterator_t
{
    auto iter = filepath_map_.find(filepath);
    if (iter == filepath_map_.end()) {
        return source_manager_t::iterator_t{filepath_map_.end()};
    }

    return source_manager_t::iterator_t{iter};
}

auto source_manager_t::find_source(std::string_view source_span) const -> source_manager_t::iterator_t
{
    // We use upper bound to find the first source file that starts after the given span.
    auto upper = source_map_.upper_bound(source_span);
    // If the upper bound is the beginning then the span is before any source file.
    // Therefore we do not have a source file for the span so we return end.
    if (upper == source_map_.begin()) {
        return source_manager_t::iterator_t{filepath_map_.end()};
    }

    // Otherwise the span is in the source file before the upper bound.
    auto iter = std::prev(upper);
    // Verify that all of the span is within the source file.
    if (iter->first.begin() <= source_span.begin() && source_span.end() <= iter->first.end()) {
        return source_manager_t::iterator_t{filepath_map_.find(iter->second.filepath())};
    }

    // If not all the span is within the source file then we return end.
    return source_manager_t::iterator_t{filepath_map_.end()};
}

auto source_manager_t::begin() const -> source_manager_t::iterator_t
{
    return source_manager_t::iterator_t{filepath_map_.begin()};
}

auto source_manager_t::end() const -> source_manager_t::iterator_t
{
    return source_manager_t::iterator_t{filepath_map_.end()};
}

} // namespace loxmocha::source
