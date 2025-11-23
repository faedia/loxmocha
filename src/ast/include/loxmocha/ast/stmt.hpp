#pragma once

#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/node.hpp"

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
class stmt_t : public node_t<expr_t /*, other statement types */> {
public:
    using node_t::node_t;
};

} // namespace loxmocha::stmt
