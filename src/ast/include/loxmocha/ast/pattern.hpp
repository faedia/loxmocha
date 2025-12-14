#pragma once

#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/ast/base.hpp"

namespace loxmocha::type {
class type_t;
}

namespace loxmocha::pattern {

class pattern_t;

/**
 * @class identifier_t
 * @brief represents an identifier pattern.
 *
 * An identifier pattern matches any value and binds it to the given name.
 *
 * They have the form:
 *
 * `identifier`
 */
class identifier_t {
public:
    identifier_t() = delete;

    identifier_t(const identifier_t&)     = delete;
    identifier_t(identifier_t&&) noexcept = default;
    ~identifier_t()                       = default;

    auto operator=(const identifier_t&) -> identifier_t&     = delete;
    auto operator=(identifier_t&&) noexcept -> identifier_t& = default;

    /**
     * @brief Constructs an identifier pattern with the given name.
     *
     * @param name The name of the identifier.
     */
    explicit identifier_t(const token_t& name) : name_(name) {}

    /**
     * @brief Get the name of the identifier pattern.
     * @return const token_t& The name of the identifier pattern.
     */
    [[nodiscard]] auto name() const -> const token_t& { return name_; }
    /**
     * @brief Get the name of the identifier pattern.
     * @return token_t& The name of the identifier pattern.
     */
    [[nodiscard]] auto name() -> token_t& { return name_; }

private:
    token_t name_;
};

/**
 * @class tag_t
 * @brief represents a tag pattern.
 *
 * A tag pattern matches a tagged value with a specific type and tag choice.
 * It consists of a type, a choice, and a sub-pattern.
 * The sub-pattern is used to match the value associated with the tag.
 *
 * They have the form:
 *
 * `"choice" type_expression "." identifier pattern`
 */
class tag_t {
public:
    tag_t() = delete;

    tag_t(const tag_t&)     = delete;
    tag_t(tag_t&&) noexcept = default;
    ~tag_t();

    auto operator=(const tag_t&) -> tag_t&     = delete;
    auto operator=(tag_t&&) noexcept -> tag_t& = default;

    /**
     * @brief Constructs a tag pattern with the given type, name, and sub-pattern.
     *
     * @param type The type of the tag.
     * @param name The name of the tag.
     * @param pattern The sub-pattern of the tag.
     */
    tag_t(safe_ptr<type::type_t>&& type, const token_t& name, safe_ptr<pattern_t>&& pattern);

    /**
     * @brief Get the type of the tag pattern.
     * @return const safe_ptr<type::type_t>& The type of the tag pattern.
     */
    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    /**
     * @brief Get the type of the tag pattern.
     * @return safe_ptr<type::type_t>& The type of the tag pattern.
     */
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

    /**
     * @brief Get the name of the tag pattern.
     * @return const token_t& The name of the tag pattern.
     */
    [[nodiscard]] auto name() const -> const token_t& { return name_; }
    /**
     * @brief Get the name of the tag pattern.
     * @return token_t& The name of the tag pattern.
     */
    [[nodiscard]] auto name() -> token_t& { return name_; }

    /**
     * @brief Get the sub-pattern of the tag pattern.
     * @return const safe_ptr<pattern_t>& The sub-pattern of the tag pattern.
     */
    [[nodiscard]] auto pattern() const -> const safe_ptr<pattern_t>& { return pattern_; }
    /**
     * @brief Get the sub-pattern of the tag pattern.
     * @return safe_ptr<pattern_t>& The sub-pattern of the tag pattern.
     */
    [[nodiscard]] auto pattern() -> safe_ptr<pattern_t>& { return pattern_; }

private:
    safe_ptr<type::type_t> type_;
    token_t                name_;
    safe_ptr<pattern_t>    pattern_;
};

class error_t {
public:
    error_t()                                      = default;
    error_t(const error_t&)                        = delete;
    error_t(error_t&&) noexcept                    = default;
    ~error_t()                                     = default;
    auto operator=(const error_t&) -> error_t&     = delete;
    auto operator=(error_t&&) noexcept -> error_t& = default;
};

/**
 * @class pattern_t
 * @brief represents a pattern.
 */
class pattern_t : public ast_node_t<identifier_t, tag_t, error_t> {
public:
    using ast_node_t::ast_node_t;

    pattern_t(const pattern_t&)     = delete;
    pattern_t(pattern_t&&) noexcept = default;

    ~pattern_t() = default;

    auto operator=(const pattern_t&) -> pattern_t&     = delete;
    auto operator=(pattern_t&&) noexcept -> pattern_t& = default;
};

} // namespace loxmocha::pattern
