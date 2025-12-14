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

struct span_less_t {
    auto operator()(const std::string_view& lhs, const std::string_view& rhs) const -> bool
    {
        return lhs.data() < rhs.data();
    }
};

class source_info_t {
public:
    source_info_t(std::filesystem::path filepath, std::string&& content);
    source_info_t() = delete;

    [[nodiscard]] auto filepath() const -> const std::filesystem::path& { return filepath_; }
    [[nodiscard]] auto content() const -> std::string_view { return content_; }
    [[nodiscard]] auto lines() const -> std::span<const std::string_view> { return lines_; }
    [[nodiscard]] auto find_location(std::string_view::iterator pos) const -> std::optional<source_location_t>;
    [[nodiscard]] auto find_span_location(std::string_view source_span) const
        -> std::optional<std::pair<source_location_t, source_location_t>>;

private:
    std::filesystem::path         filepath_;
    std::string                   content_;
    std::vector<std::string_view> lines_;
};

class source_view_t {
public:
    source_view_t(std::filesystem::path filepath, std::string_view content, std::span<const std::string_view> lines)
        : filepath_(std::move(filepath)), content_(content), lines_(lines)
    {
    }

    [[nodiscard]] auto filepath() const -> const std::filesystem::path& { return filepath_; }
    [[nodiscard]] auto content() const -> std::string_view { return content_; }
    [[nodiscard]] auto lines() const -> std::span<const std::string_view> { return lines_; }
    [[nodiscard]] auto find_location(std::string_view::iterator pos) const -> std::optional<source_location_t>;
    [[nodiscard]] auto find_span_location(std::string_view source_span) const
        -> std::optional<std::pair<source_location_t, source_location_t>>;

private:
    std::filesystem::path             filepath_;
    std::string_view                  content_;
    std::span<const std::string_view> lines_;
};

class source_manager_t {
public:
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

    auto emplace(safe_ptr<source_info_t>&& source_info) -> std::pair<iterator_t, bool>;

    [[nodiscard]] auto find_source(std::string_view source_span) const -> iterator_t;
    [[nodiscard]] auto find_source(const std::filesystem::path& filepath) const -> iterator_t;

    [[nodiscard]] auto begin() const -> iterator_t;
    [[nodiscard]] auto end() const -> iterator_t;

private:
    std::unordered_map<std::filesystem::path, safe_ptr<source_info_t>> filepath_map_;
    std::map<std::string_view, source_info_t&, span_less_t>            source_map_;
};

} // namespace loxmocha::source
