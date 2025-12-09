#pragma once

#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/node.hpp"

#include <variant>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::decl {
class decl_t;
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
    ~expr_t();

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
 * @brief Represents an assignment statement.
 */
class assign_t {
public:
    assign_t() = delete;

    assign_t(const assign_t&)     = delete;
    assign_t(assign_t&&) noexcept = default;
    ~assign_t();

    auto operator=(const assign_t&) -> assign_t&     = delete;
    auto operator=(assign_t&&) noexcept -> assign_t& = default;

    /**
     * @brief Constructs an assignment statement with the given target and value expressions.
     *
     * @param target The target expression to assign to.
     * @param value The value expression to assign.
     */
    explicit assign_t(safe_ptr<expr::expr_t>&& target, safe_ptr<expr::expr_t>&& value);

    /**
     * @brief Get the target expression of the assignment.
     * @return const safe_ptr<expr::expr_t>& The target expression of the assignment.
     */
    [[nodiscard]] auto target() const -> const safe_ptr<expr::expr_t>& { return target_; }
    /**
     * @brief Get the target expression of the assignment.
     * @return safe_ptr<expr::expr_t>& The target expression of the assignment.
     */
    [[nodiscard]] auto target() -> safe_ptr<expr::expr_t>& { return target_; }

    /**
     * @brief Get the value expression of the assignment.
     * @return const safe_ptr<expr::expr_t>& The value expression of the assignment.
     */
    [[nodiscard]] auto value() const -> const safe_ptr<expr::expr_t>& { return value_; }
    /**
     * @brief Get the value expression of the assignment.
     * @return safe_ptr<expr::expr_t>& The value expression of the assignment.
     */
    [[nodiscard]] auto value() -> safe_ptr<expr::expr_t>& { return value_; }

private:
    safe_ptr<expr::expr_t> target_;
    safe_ptr<expr::expr_t> value_;
};

/**
 * @brief Represents a declaration statement.
 */
class decl_t {
public:
    decl_t() = delete;

    decl_t(const decl_t&)     = delete;
    decl_t(decl_t&&) noexcept = default;
    ~decl_t();

    auto operator=(const decl_t&) -> decl_t&     = delete;
    auto operator=(decl_t&&) noexcept -> decl_t& = default;

    /**
     * @brief Constructs a declaration statement from a declaration.
     *
     * @param declaration The declaration to wrap in the statement.
     */
    explicit decl_t(safe_ptr<decl::decl_t>&& declaration);

    /**
     * @brief Get the declaration contained in the statement.
     * @return const safe_ptr<decl::decl_t>& The declaration contained in the statement.
     */
    [[nodiscard]] auto declaration() const -> const safe_ptr<decl::decl_t>& { return declaration_; }
    /**
     * @brief Get the declaration contained in the statement.
     * @return safe_ptr<decl::decl_t>& The declaration contained in the statement.
     */
    [[nodiscard]] auto declaration() -> safe_ptr<decl::decl_t>& { return declaration_; }

private:
    safe_ptr<decl::decl_t> declaration_;
};

/**
 * @class stmt_t
 * @brief represents a statement
 */
class stmt_t : public node_t<expr_t, assign_t, decl_t> {
public:
    using node_t::node_t;

    stmt_t(const stmt_t&)     = delete;
    stmt_t(stmt_t&&) noexcept = default;

    ~stmt_t() = default;

    auto operator=(const stmt_t&) -> stmt_t&     = delete;
    auto operator=(stmt_t&&) noexcept -> stmt_t& = default;
};

} // namespace loxmocha::stmt
