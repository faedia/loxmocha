#pragma once

#include "loxmocha/memory/safe_pointer.hpp"

#include <variant>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::stmt {

/**
 * @brief Represents an expression statement.
 */
class expr_t {
public:
    expr_t() = delete;

    expr_t(const expr_t&)     = delete;
    expr_t(expr_t&&) noexcept = default;
    ~expr_t()                 = default;

    /**
     * @brief Constructs an expression statement from an expression.
     *
     * @param expr The expression to wrap in the statement.
     */
    explicit expr_t(safe_ptr<expr::expr_t>&& expr);

    auto operator=(const expr_t&) -> expr_t&     = delete;
    auto operator=(expr_t&&) noexcept -> expr_t& = default;

    /**
     * @brief Get the expression contained in the statement.
     * @return const safe_ptr<expr::expr_t>& The expression contained in the statement.
     */
    [[nodiscard]] auto expr() const -> const safe_ptr<expr::expr_t>& { return expr_; }
    /**
     * @brief Get the expression contained in the statement.
     * @return safe_ptr<expr::expr_t>& The expression contained in the statement.
     */
    [[nodiscard]] auto expr() -> safe_ptr<expr::expr_t>& { return expr_; }

private:
    safe_ptr<expr::expr_t> expr_;
};

/**
 * @class stmt_t
 * @brief represents a statement
 */
class stmt_t {
public:
    stmt_t() = delete;

    stmt_t(const stmt_t&)     = delete;
    stmt_t(stmt_t&&) noexcept = default;
    ~stmt_t()                 = default;

    auto operator=(const stmt_t&) -> stmt_t&     = delete;
    auto operator=(stmt_t&&) noexcept -> stmt_t& = default;
    template<typename T>
    auto operator=(T&& value) noexcept -> stmt_t&
    {
        stmt_ = std::forward<T>(value);
        return *this;
    }

    /**
     * @brief Constructs an statement from a specific statement kind.
     *
     * @tparam T The specific statement kind.
     * @param value The specific statement kind to construct from.
     */
    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    stmt_t(T&& value) noexcept : stmt_(std::forward<T>(value))
    {
    }

    /**
     * @brief Constructs a statement in-place from the specified kind and arguments.
     *
     * @tparam T The specific statement kind.
     * @tparam Args The types of the arguments to construct the statement.
     * @param args The arguments to construct the statement.
     */
    template<typename T, typename... Args>
    // NOLINTNEXTLINE(readability-named-parameter,hicpp-named-parameter)
    explicit stmt_t(std::in_place_type_t<T>, Args&&... args) : stmt_(std::in_place_type<T>, std::forward<Args>(args)...)
    {
    }

    /**
     * @brief Check if the statement holds a specific statement kind.
     *
     * @tparam T The statement kind to check.
     * @return true if the statement holds the specified kind, false otherwise.
     */
    template<typename T>
    [[nodiscard]] auto is() const -> bool
    {
        return std::holds_alternative<T>(stmt_);
    }

    /**
     * @brief Get the statement as a specific statement kind.
     *
     * @tparam T The statement kind to get.
     * @return T& The statement as the specified kind.
     */
    template<typename T>
    [[nodiscard]] auto as() -> T&
    {
        return std::get<T>(stmt_);
    }

    /**
     * @brief Get the statement as a specific statement kind.
     * @tparam T The statement kind to get.
     * @return const T& The statement as the specified kind.
     */
    template<typename T>
    [[nodiscard]] auto as() const -> const T&
    {
        return std::get<T>(stmt_);
    }

    /**
     * @brief Visit the statement with a visitor.
     *
     * @tparam Visitor The type of the visitor.
     * @param visitor The visitor to apply to the statement.
     * @return decltype(auto) The result of applying the visitor to the statement.
     */
    template<typename Visitor, typename... Args>
    auto visit(Visitor&& visitor, Args&&... args) const
    {
        return std::visit([&visitor, &args...](auto&& arg) { return visitor(arg, std::forward<Args>(args)...); },
                          stmt_);
    }

    /**
     * @brief Visit the statement with a visitor.
     *
     * @tparam Visitor The type of the visitor.
     * @param visitor The visitor to apply to the statement.
     * @return decltype(auto) The result of applying the visitor to the statement.
     */
    template<typename Visitor, typename... Args>
    auto visit(Visitor&& visitor, Args&&... args)
    {
        return std::visit([&visitor, &args...](auto&& arg) { return visitor(arg, std::forward<Args>(args)...); },
                          stmt_);
    }

private:
    std::variant<expr_t /*, other statement types */> stmt_;
};
} // namespace loxmocha::stmt
