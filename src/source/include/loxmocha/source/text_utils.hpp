#pragma once

#include <cstddef>
#include <string_view>

namespace loxmocha::source {

/**
 * @brief Strips leading and trailing horizontal whitespace from a source line.
 * @param line The line to trim.
 * @return A view of @p line with surrounding whitespace removed.
 */
[[nodiscard]] auto trim(std::string_view line) -> std::string_view;

/**
 * @brief Builds a caret marker line pointing at a column, for diagnostic output.
 * @param column The 1-based column the caret should point at.
 * @return A string of the form "   ^" with @p column - 1 leading spaces.
 */
[[nodiscard]] auto caret_marker(std::size_t column) -> std::string_view;

/**
 * @brief Returns the first @p count characters of a line, for truncated output.
 * @param line The line to truncate.
 * @param count The number of characters to keep.
 */
[[nodiscard]] auto prefix(std::string_view line, std::size_t count) -> std::string_view;

} // namespace loxmocha::source
