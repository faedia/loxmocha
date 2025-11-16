#pragma once

#include "loxmocha/memory/safe_pointer.hpp"

#include <variant>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::stmt {

class expr_t {
public:
    expr_t() = delete;

    expr_t(const expr_t&)     = delete;
    expr_t(expr_t&&) noexcept = default;
    ~expr_t()                 = default;

    explicit expr_t(safe_ptr<expr::expr_t>&& expr);

    auto operator=(const expr_t&) -> expr_t&     = delete;
    auto operator=(expr_t&&) noexcept -> expr_t& = default;

    [[nodiscard]] auto expr() const -> const safe_ptr<expr::expr_t>& { return expr_; }
    [[nodiscard]] auto expr() -> safe_ptr<expr::expr_t>& { return expr_; }

private:
    safe_ptr<expr::expr_t> expr_;
};

class stmt_t {
public:
    stmt_t() = delete;

    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    stmt_t(T&& value) noexcept : stmt_(std::forward<T>(value))
    {
    }
    template<typename T, typename... Args>
    // NOLINTNEXTLINE(readability-named-parameter,hicpp-named-parameter)
    explicit stmt_t(std::in_place_type_t<T>, Args&&... args) : stmt_(std::in_place_type<T>, std::forward<Args>(args)...)
    {
    }

    stmt_t(const stmt_t&)     = delete;
    stmt_t(stmt_t&&) noexcept = default;
    ~stmt_t()                 = default;

    template<typename T>
    auto operator=(T&& stmt) noexcept -> stmt_t&
    {
        stmt_ = std::forward<T>(stmt);
        return *this;
    }
    auto operator=(const stmt_t&) -> stmt_t&     = delete;
    auto operator=(stmt_t&&) noexcept -> stmt_t& = default;

    template<typename T>
    [[nodiscard]] auto is() const -> bool
    {
        return std::holds_alternative<T>(stmt_);
    }

    template<typename T>
    [[nodiscard]] auto as() -> T&
    {
        return std::get<T>(stmt_);
    }

    template<typename T>
    [[nodiscard]] auto as() const -> const T&
    {
        return std::get<T>(stmt_);
    }

    template<typename Visitor>
    [[nodiscard]] auto visit(Visitor&& visitor) -> decltype(auto)
    {
        return std::visit(std::forward<Visitor>(visitor), stmt_);
    }

private:
    std::variant<expr_t /*, other statement types */> stmt_;
};
} // namespace loxmocha::stmt
