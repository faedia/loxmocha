#pragma once

#include <compare>
#include <cstddef>
#include <deque>
#include <string>
#include <unordered_map>

namespace loxmocha::lexer {

/**
 * @class ident_t
 * @brief Represents an identifier that was lexed from the input stream.
 * Identifiers with the same name will have the same ID.
 */
class ident_t {
public:
    ident_t()                                  = default;
    ident_t(const ident_t&)                    = default;
    ident_t(ident_t&&)                         = default;
    auto operator=(const ident_t&) -> ident_t& = default;
    auto operator=(ident_t&&) -> ident_t&      = default;
    ~ident_t()                                 = default;

    explicit ident_t(std::size_t id) : id_{id} {}

    [[nodiscard]] auto operator<=>(const ident_t&) const -> std::strong_ordering = default;

    [[nodiscard]] auto id() const -> std::size_t { return id_; }

private:
    std::size_t id_{};
};

/**
 * @class ident_map_t
 * @brief Maps identifier strings to unique ident_t IDs and vice versa.
 */
class ident_map_t {
public:
    /**
     * @brief Get the identifier string corresponding to the given identifier.
     *
     * @param id The identifier.
     * @return const std::string& The identifier string.
     */
    [[nodiscard]] auto operator[](const ident_t& id) const -> const std::string& { return id_to_ident_[id.id()]; }

    /**
     * @brief Insert an identifier string into the map and get its corresponding identifier.
     * If the identifier already exists, returns the existing identifier.
     *
     * @param ident The identifier string.
     * @return ident_t The corresponding identifier.
     */
    [[nodiscard]] auto emplace(std::string&& ident) -> ident_t
    {
        if (const auto iter = ident_to_id_.find(ident); iter != ident_to_id_.end()) {
            return iter->second;
        }

        const ident_t next_id{id_to_ident_.size()};
        const auto&   iter = id_to_ident_.emplace_back(std::move(ident));
        return ident_to_id_.emplace(iter, next_id).first->second;
    }

private:
    // Note: the unordered_map uses string_view's of the strings stored in the deque.
    // This is because we only ever add new strings to the deque, however if we need to remove
    // them we will need to revisit this.
    std::deque<std::string>                       id_to_ident_;
    std::unordered_map<std::string_view, ident_t> ident_to_id_;
};

} // namespace loxmocha::lexer
