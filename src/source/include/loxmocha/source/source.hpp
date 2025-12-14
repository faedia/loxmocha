#pragma once

#include "loxmocha/memory/safe_pointer.hpp"

#include <cstddef>
#include <filesystem>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace loxmocha::source {

struct source_location_t {
    std::string_view line_span;
    std::size_t      line;
    std::size_t      column;
};

/**
 * @class source_info_t
 * @brief Represents information about a source file, including its path, content, and line mappings.
 * This class is responsible for owning the source content and providing ways to map pointers in the content to source
 * locations.
 */
class source_info_t {
public:
    /**
     * @brief Constructs a source_info_t with the given file path and content.
     * @param filepath The file path of the source file.
     * @param content The content of the source file. The content is moved into the source_info_t.
     */
    source_info_t(std::filesystem::path filepath, std::string&& content);
    source_info_t() = delete;

    source_info_t(const source_info_t&)     = delete;
    source_info_t(source_info_t&&) noexcept = default;
    ~source_info_t()                        = default;

    auto operator=(const source_info_t&) -> source_info_t&     = delete;
    auto operator=(source_info_t&&) noexcept -> source_info_t& = default;

    /**
     * @brief Get the file path of the source file.
     * @return const std::filesystem::path& The file path of the source file.
     */
    [[nodiscard]] auto filepath() const -> const std::filesystem::path& { return filepath_; }

    /**
     * @brief Get the content of the source file.
     * @return std::string_view The content of the source file.
     */
    [[nodiscard]] auto content() const -> std::string_view { return content_; }

    /** @brief Get the lines of the source file.
     * @return std::span<const std::string_view> The lines of the source file.
     */
    [[nodiscard]] auto lines() const -> std::span<const std::string_view> { return lines_; }

    /**
     * @brief Find the source location corresponding to a position in the source content.
     * @param pos An iterator pointing to a position in the source content.
     * @return std::optional<source_location_t> The source location if found, or std::nullopt if not found.
     */
    [[nodiscard]] auto find_location(std::string_view::iterator pos) const -> std::optional<source_location_t>;

    /**
     * @brief Find the source locations corresponding to a span in the source content.
     * @param source_span A string view representing a span in the source content.
     * @return std::optional<std::pair<source_location_t, source_location_t>> A pair of source locations representing
     * the start and end of the span if found, or std::nullopt if not found.
     */
    [[nodiscard]] auto find_span_location(std::string_view source_span) const
        -> std::optional<std::pair<source_location_t, source_location_t>>;

private:
    std::filesystem::path         filepath_;
    std::string                   content_;
    std::vector<std::string_view> lines_;
};

/**
 * @class source_view_t
 * @brief Represents a view into a source file, providing access to its path, content, and line mappings.
 * This class does not own the source content, instead the content is owned by a source_info_t.
 * This provides the same mapping functionality as source_info_t.
 * Iteration order of source files is not guaranteed to be in any particular order.
 */
class source_view_t {
public:
    /**
     * @brief Constructs a source_view_t with the given file path, content, and line mappings.
     * @param filepath The file path of the source file.
     * @param content The content of the source file.
     * @param lines The line mappings of the source file.
     */
    source_view_t(std::filesystem::path filepath, std::string_view content, std::span<const std::string_view> lines)
        : filepath_(std::move(filepath)), content_(content), lines_(lines)
    {
    }

    source_view_t()                                            = delete;
    source_view_t(const source_view_t&)                        = default;
    source_view_t(source_view_t&&) noexcept                    = default;
    ~source_view_t()                                           = default;
    auto operator=(const source_view_t&) -> source_view_t&     = default;
    auto operator=(source_view_t&&) noexcept -> source_view_t& = default;

    /**
     * @brief Get the file path of the source file.
     * @return const std::filesystem::path& The file path of the source file.
     */
    [[nodiscard]] auto filepath() const -> const std::filesystem::path& { return filepath_; }

    /**
     * @brief Get the content of the source file.
     * @return std::string_view The content of the source file.
     */
    [[nodiscard]] auto content() const -> std::string_view { return content_; }

    /**
     * @brief Get the lines of the source file.
     * @return std::span<const std::string_view> The lines of the source file.
     */
    [[nodiscard]] auto lines() const -> std::span<const std::string_view> { return lines_; }

    /**
     * @brief Find the source location corresponding to a position in the source content.
     * @param pos An iterator pointing to a position in the source content.
     * @return std::optional<source_location_t> The source location if found, or std::nullopt if not found.
     */
    [[nodiscard]] auto find_location(std::string_view::iterator pos) const -> std::optional<source_location_t>;

    /**
     * @brief Find the source locations corresponding to a span in the source content.
     * @param source_span A string view representing a span in the source content.
     * @return std::optional<std::pair<source_location_t, source_location_t>> A pair of source locations representing
     * the start and end of the span if found, or std::nullopt if not found.
     */
    [[nodiscard]] auto find_span_location(std::string_view source_span) const
        -> std::optional<std::pair<source_location_t, source_location_t>>;

private:
    std::filesystem::path             filepath_;
    std::string_view                  content_;
    std::span<const std::string_view> lines_;
};

/**
 * @class source_manager_t
 * @brief Manages source files and provides access to views of those source files.
 * This class owns the source_info_t that represent and own the source content.
 * This allows retrieval of source views by either file path, or by a span within the source content.
 */
class source_manager_t {
public:
    /**
     * @class iterator_t
     * @brief Iterator into the source files manages by source_manager_t.
     * This iterator provides an access to source_view_t for the source file it points to.
     */
    class iterator_t {
    public:
        auto operator++() -> iterator_t&
        {
            ++internal_iterator_;
            return *this;
        }

        [[nodiscard]] auto operator*() const -> source_view_t { return this->view(); }
        [[nodiscard]] auto operator->() const -> source_view_t { return this->view(); }
        [[nodiscard]] auto operator==(const iterator_t& other) const -> bool
        {
            return internal_iterator_ == other.internal_iterator_;
        }

        [[nodiscard]] auto operator!=(const iterator_t& other) const -> bool { return !(*this == other); }

        [[nodiscard]] auto view() const -> source_view_t
        {
            return source_view_t{internal_iterator_->second->filepath(),
                                 internal_iterator_->second->content(),
                                 internal_iterator_->second->lines()};
        }

    private:
        friend class source_manager_t;
        explicit iterator_t(std::unordered_map<std::filesystem::path, safe_ptr<source_info_t>>::const_iterator iter)
            : internal_iterator_(iter)
        {
        }
        std::unordered_map<std::filesystem::path, safe_ptr<source_info_t>>::const_iterator internal_iterator_;
    };

    source_manager_t() = default;

    source_manager_t(const source_manager_t&)     = delete;
    source_manager_t(source_manager_t&&) noexcept = default;
    ~source_manager_t()                           = default;

    auto operator=(const source_manager_t&) -> source_manager_t&     = delete;
    auto operator=(source_manager_t&&) noexcept -> source_manager_t& = default;

    /**
     * @brief Emplaces a new source file into the source manager.
     * @param source_info The source_info_t owning the source content to emplace.
     * @return std::pair<iterator_t, bool> A pair containing the iterator the emplaced source file, and bool indicating
     * if the source file was emplaced or if it already existed. If the source file already existed, the iterator points
     * to the existing source file and the new source file was not emplaced.
     * */
    auto emplace(safe_ptr<source_info_t>&& source_info) -> std::pair<iterator_t, bool>;

    /**
     * @brief Finds a source file by a span within its content.
     * @param source_span The span within the source content to find the source file for.
     * @return iterator_t An iterator to the found source file, or end if not found.
     */
    [[nodiscard]] auto find_source(std::string_view source_span) const -> iterator_t;
    /**
     * @brief Finds a source file by its file path.
     * @param filepath The file path of the source file to find.
     * @return iterator_t An iterator to the found source file, or end if not found.
     */
    [[nodiscard]] auto find_source(const std::filesystem::path& filepath) const -> iterator_t;

    /**
     * @brief Gets an iterator to the beginning of the source files.
     * @return iterator_t An iterator to the beginning of the source files.
     */
    [[nodiscard]] auto begin() const -> iterator_t;
    /**
     * @brief Gets an iterator to the end of the source files.
     * @return iterator_t An iterator to the end of the source files.
     */
    [[nodiscard]] auto end() const -> iterator_t;

private:
    struct span_less_t {
        auto operator()(const std::string_view& lhs, const std::string_view& rhs) const -> bool
        {
            return lhs.data() < rhs.data();
        }
    };
    std::unordered_map<std::filesystem::path, safe_ptr<source_info_t>> filepath_map_;
    std::map<std::string_view, source_info_t&, span_less_t>            source_map_;
};

} // namespace loxmocha::source
