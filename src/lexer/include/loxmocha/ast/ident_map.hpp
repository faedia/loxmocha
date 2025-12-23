#pragma once

#include <compare>
#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

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
     * @return const std::string_view& The identifier string.
     */
    [[nodiscard]] auto operator[](const ident_t& id) const -> const std::string_view& { return id_to_ident_[id.id()]; }

    /**
     * @brief Insert an identifier string into the map and get its corresponding identifier.
     * If the identifier already exists, returns the existing identifier.
     *
     * @param ident The identifier string.
     * @return ident_t The corresponding identifier.
     */
    [[nodiscard]] auto insert(const std::string& ident) -> ident_t
    {
        // If the identifier already exists then return existing ID.
        auto iter = ident_to_id_.find(ident);
        if (iter != ident_to_id_.end()) {
            return iter->second;
        }
        const ident_t id{id_to_ident_.size()};
        const auto [it, _] = ident_to_id_.emplace(ident, id);
        id_to_ident_.emplace_back(it->first);
        return it->second;
    }

private:
    std::vector<std::string_view>            id_to_ident_;
    std::unordered_map<std::string, ident_t> ident_to_id_;
};

} // namespace loxmocha::lexer
