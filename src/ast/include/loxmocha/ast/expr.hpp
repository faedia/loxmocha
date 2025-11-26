#pragma once

#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/node.hpp"

#include <utility>
#include <variant>
#include <vector>

namespace loxmocha::stmt {
class stmt_t;
}

namespace loxmocha::type {
class type_t;
}

namespace loxmocha::expr {

class expr_t;

/**
 * @class literal_t
 * @brief represents a literal expression.
 */
class literal_t {
public:
    literal_t() = delete;

    literal_t(const literal_t&)     = default;
    literal_t(literal_t&&) noexcept = default;
    ~literal_t()                    = default;

    auto operator=(const literal_t&) -> literal_t&     = default;
    auto operator=(literal_t&&) noexcept -> literal_t& = default;

    /**
     * @brief Constructs a literal expression with the given token value.
     *
     * @param value The token representing the literal value.
     */
    explicit literal_t(const token_t& value) : value_(value) {}

    /**
     * @brief Get the token value of the literal expression.
     * @return const token_t& The token value of the literal expression.
     */
    [[nodiscard]] auto value() const -> const token_t& { return value_; }
    /**
     * @brief Get the token value of the literal expression.
     * @return token_t& The token value of the literal expression.
     */
    [[nodiscard]] auto value() -> token_t& { return value_; }

private:
    token_t value_;
};

/**
 * @class identifier_t
 * @brief represents an identifier expression.
 */
class identifier_t {
public:
    identifier_t() = delete;

    identifier_t(const identifier_t&)     = default;
    identifier_t(identifier_t&&) noexcept = default;
    ~identifier_t()                       = default;

    auto operator=(const identifier_t&) -> identifier_t&     = default;
    auto operator=(identifier_t&&) noexcept -> identifier_t& = default;

    /**
     * @brief Constructs an identifier expression with the given token name.
     *
     * @param name The token representing the identifier name.
     */
    explicit identifier_t(const token_t& name) : name_(name) {}

    /**
     * @brief Get the token name of the identifier expression.
     * @return const token_t& The token name of the identifier expression.
     */
    [[nodiscard]] auto name() const -> const token_t& { return name_; }
    /**
     * @brief Get the token name of the identifier expression.
     * @return token_t& The token name of the identifier expression.
     */
    [[nodiscard]] auto name() -> token_t& { return name_; }

private:
    token_t name_;
};

/**
 * @class binary_t
 * @brief represents a binary expression.
 */
class binary_t {
public:
    binary_t() = delete;

    binary_t(const binary_t&)     = delete;
    binary_t(binary_t&&) noexcept = default;
    ~binary_t()                   = default;

    auto operator=(const binary_t&) -> binary_t&     = delete;
    auto operator=(binary_t&&) noexcept -> binary_t& = default;

    /**
     * @brief Constructs a binary expression with the given operator and operands.
     *
     * @param op The token representing the binary operator.
     * @param left The left operand expression.
     * @param right The right operand expression.
     */
    binary_t(const token_t& op, safe_ptr<expr_t>&& left, safe_ptr<expr_t>&& right);

    /**
     * @brief Get the operator token of the binary expression.
     * @return const token_t& The operator token of the binary expression.
     */
    [[nodiscard]] auto op() const -> const token_t& { return op_; }
    /**
     * @brief Get the operator token of the binary expression.
     * @return token_t& The operator token of the binary expression.
     */
    [[nodiscard]] auto op() -> token_t& { return op_; }

    /**
     * @brief Get the left operand expression of the binary expression.
     * @return const safe_ptr<expr_t>& The left operand expression of the binary expression.
     */
    [[nodiscard]] auto left() const -> const safe_ptr<expr_t>& { return left_; }
    /**
     * @brief Get the left operand expression of the binary expression.
     * @return safe_ptr<expr_t>& The left operand expression of the binary expression.
     */
    [[nodiscard]] auto left() -> safe_ptr<expr_t>& { return left_; }

    /**
     * @brief Get the right operand expression of the binary expression.
     * @return const safe_ptr<expr_t>& The right operand expression of the binary expression.
     */
    [[nodiscard]] auto right() const -> const safe_ptr<expr_t>& { return right_; }
    /**
     * @brief Get the right operand expression of the binary expression.
     * @return safe_ptr<expr_t>& The right operand expression of the binary expression.
     */
    [[nodiscard]] auto right() -> safe_ptr<expr_t>& { return right_; }

private:
    token_t          op_;
    safe_ptr<expr_t> left_;
    safe_ptr<expr_t> right_;
};

/**
 * @class unary_t
 * @brief represents a unary expression.
 */
class unary_t {
public:
    unary_t() = delete;

    unary_t(const unary_t&)     = delete;
    unary_t(unary_t&&) noexcept = default;
    ~unary_t()                  = default;

    auto operator=(const unary_t&) -> unary_t&     = delete;
    auto operator=(unary_t&&) noexcept -> unary_t& = default;

    /**
     * @brief Constructs a unary expression with the given operator and operand.
     *
     * @param op The token representing the unary operator.
     * @param operand The operand expression.
     */
    unary_t(const token_t& op, safe_ptr<expr_t>&& operand);

    /**
     * @brief Get the operator token of the unary expression.
     * @return const token_t& The operator token of the unary expression.
     */
    [[nodiscard]] auto op() const -> const token_t& { return op_; }
    /**
     * @brief Get the operator token of the unary expression.
     * @return token_t& The operator token of the unary expression.
     */
    [[nodiscard]] auto op() -> token_t& { return op_; }

    /**
     * @brief Get the operand expression of the unary expression.
     * @return const safe_ptr<expr_t>& The operand expression of the unary expression.
     */
    [[nodiscard]] auto operand() const -> const safe_ptr<expr_t>& { return operand_; }
    /**
     * @brief Get the operand expression of the unary expression.
     * @return safe_ptr<expr_t>& The operand expression of the unary expression.
     */
    [[nodiscard]] auto operand() -> safe_ptr<expr_t>& { return operand_; }

private:
    token_t          op_;
    safe_ptr<expr_t> operand_;
};

/**
 * @class is_t
 * @brief represents an "is" type check expression.
 */
class is_t {
public:
    is_t() = delete;

    is_t(const is_t&)     = delete;
    is_t(is_t&&) noexcept = default;
    ~is_t();

    auto operator=(const is_t&) -> is_t&     = delete;
    auto operator=(is_t&&) noexcept -> is_t& = default;

    /**
     * @brief Constructs an "is" type check expression with the given expression and type.
     *
     * @param expr The expression to check.
     * @param type The type to check against.
     */
    is_t(safe_ptr<expr_t>&& expr, safe_ptr<type::type_t>&& type);

    /**
     * @brief Get the expression being checked.
     * @return const safe_ptr<expr_t>& The expression being checked.
     */
    [[nodiscard]] auto expr() const -> const safe_ptr<expr_t>& { return expr_; }
    /**
     * @brief Get the expression being checked.
     * @return safe_ptr<expr_t>& The expression being checked.
     */
    [[nodiscard]] auto expr() -> safe_ptr<expr_t>& { return expr_; }

    /**
     * @brief Get the type being checked against.
     * @return const safe_ptr<type::type_t>& The type being checked against.
     */
    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    /**
     * @brief Get the type being checked against.
     * @return safe_ptr<type::type_t>& The type being checked against.
     */
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    safe_ptr<expr_t>       expr_;
    safe_ptr<type::type_t> type_;
};

/**
 * @class cast_t
 * @brief represents an "as" type cast expression.
 */
class cast_t {
public:
    cast_t() = delete;

    cast_t(const cast_t&)     = delete;
    cast_t(cast_t&&) noexcept = default;
    ~cast_t();

    auto operator=(const cast_t&) -> cast_t&     = delete;
    auto operator=(cast_t&&) noexcept -> cast_t& = default;

    /**
     * @brief Constructs an "as" type cast expression with the given expression and type.
     *
     * @param expr The expression to cast.
     * @param type The type to cast to.
     */
    cast_t(safe_ptr<expr_t>&& expr, safe_ptr<type::type_t>&& type);

    /**
     * @brief Get the expression being cast.
     * @return const safe_ptr<expr_t>& The expression being cast.
     */
    [[nodiscard]] auto expr() const -> const safe_ptr<expr_t>& { return expr_; }
    /**
     * @brief Get the expression being cast.
     * @return safe_ptr<expr_t>& The expression being cast.
     */
    [[nodiscard]] auto expr() -> safe_ptr<expr_t>& { return expr_; }

    /**
     * @brief Get the type being cast to.
     * @return const safe_ptr<type::type_t>& The type being cast to.
     */
    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    /**
     * @brief Get the type being cast to.
     * @return safe_ptr<type::type_t>& The type being cast to.
     */
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    safe_ptr<expr_t>       expr_;
    safe_ptr<type::type_t> type_;
};

/**
 * @class array_t
 * @brief represents an array expression.
 */
class array_t {
public:
    array_t() = delete;

    array_t(const array_t&)     = delete;
    array_t(array_t&&) noexcept = default;
    ~array_t()                  = default;

    auto operator=(const array_t&) -> array_t&     = delete;
    auto operator=(array_t&&) noexcept -> array_t& = default;

    /**
     * @brief Constructs an array expression with the given elements.
     *
     * @param elements The elements of the array.
     */
    explicit array_t(std::vector<expr_t>&& elements);

    /**
     * @brief Get the elements of the array expression.
     * @return const std::vector<expr_t>& The elements of the array expression.
     */
    [[nodiscard]] auto elements() const -> const std::vector<expr_t>& { return elements_; }
    [[nodiscard]] auto elements() -> std::vector<expr_t>& { return elements_; }

private:
    std::vector<expr_t> elements_;
};

/**
 * @class tuple_t
 * @brief represents a tuple expression.
 */
class tuple_t {
public:
    tuple_t() = delete;

    tuple_t(const tuple_t&)     = delete;
    tuple_t(tuple_t&&) noexcept = default;
    ~tuple_t()                  = default;

    auto operator=(const tuple_t&) -> tuple_t&     = delete;
    auto operator=(tuple_t&&) noexcept -> tuple_t& = default;

    /**
     * @brief Constructs a tuple expression with the given elements.
     *
     * @param elements The elements of the tuple.
     */
    explicit tuple_t(std::vector<expr_t>&& elements);

    /**
     * @brief Get the elements of the tuple expression.
     * @return const std::vector<expr_t>& The elements of the tuple expression.
     */
    [[nodiscard]] auto elements() const -> const std::vector<expr_t>& { return elements_; }
    /**
     * @brief Get the elements of the tuple expression.
     * @return std::vector<expr_t>& The elements of the tuple expression.
     */
    [[nodiscard]] auto elements() -> std::vector<expr_t>& { return elements_; }

private:
    std::vector<expr_t> elements_;
};

/**
 * @class record_t
 * @brief represents a record expression.
 */
class record_t {
public:
    record_t() = delete;

    record_t(const record_t&)     = delete;
    record_t(record_t&&) noexcept = default;
    ~record_t()                   = default;

    auto operator=(const record_t&) -> record_t&     = delete;
    auto operator=(record_t&&) noexcept -> record_t& = default;

    /**
     * @brief Struct representing a field in the record.
     */
    struct field_t;

    /**
     * @brief Constructs a record expression with the given fields.
     * @param fields The fields of the record.
     */
    explicit record_t(std::vector<field_t>&& fields);

    /**
     * @brief Get the fields of the record expression.
     * @return const std::vector<Field>& The fields of the record expression.
     */
    [[nodiscard]] auto fields() const -> const std::vector<field_t>& { return fields_; }
    /**
     * @brief Get the fields of the record expression.
     * @return std::vector<Field>& The fields of the record expression.
     */
    [[nodiscard]] auto fields() -> std::vector<field_t>& { return fields_; }

private:
    std::vector<field_t> fields_;
};

/**
 * @class index_t
 * @brief represents an array index expression.
 */
class index_t {
public:
    index_t() = delete;

    index_t(const index_t&)     = delete;
    index_t(index_t&&) noexcept = default;
    ~index_t()                  = default;

    auto operator=(const index_t&) -> index_t&     = delete;
    auto operator=(index_t&&) noexcept -> index_t& = default;

    /**
     * @brief Constructs an array index expression with the given base expression and index expression.
     *
     * @param base The base expression being indexed.
     * @param index The index expression.
     */
    index_t(safe_ptr<expr_t>&& base, safe_ptr<expr_t>&& index);

    /**
     * @brief Get the base expression being indexed.
     * @return const safe_ptr<expr_t>& The base expression being indexed.
     */
    [[nodiscard]] auto base() const -> const safe_ptr<expr_t>& { return base_; }
    /**
     * @brief Get the base expression being indexed.
     * @return safe_ptr<expr_t>& The base expression being indexed.
     */
    [[nodiscard]] auto base() -> safe_ptr<expr_t>& { return base_; }

    /**
     * @brief Get the index expression.
     * @return const safe_ptr<expr_t>& The index expression.
     */
    [[nodiscard]] auto index() const -> const safe_ptr<expr_t>& { return index_; }
    /**
     * @brief Get the index expression.
     * @return safe_ptr<expr_t>& The index expression.
     */
    [[nodiscard]] auto index() -> safe_ptr<expr_t>& { return index_; }

private:
    safe_ptr<expr_t> base_;
    safe_ptr<expr_t> index_;
};

/**
 * @class field_t
 * @brief represents a field access expression of a record.
 */
class field_t {
public:
    field_t() = delete;

    field_t(const field_t&)     = delete;
    field_t(field_t&&) noexcept = default;
    ~field_t()                  = default;

    auto operator=(const field_t&) -> field_t&     = delete;
    auto operator=(field_t&&) noexcept -> field_t& = default;

    /**
     * @brief Constructs a field access expression with the given base expression and field name.
     *
     * @param base The base expression representing the record.
     * @param field_name The token representing the field name to access.
     */
    field_t(safe_ptr<expr_t>&& base, const token_t& field_name);

    /**
     * @brief Get the base expression representing the record.
     * @return const safe_ptr<expr_t>& The base expression representing the record.
     */
    [[nodiscard]] auto base() const -> const safe_ptr<expr_t>& { return base_; }
    /**
     * @brief Get the base expression representing the record.
     * @return safe_ptr<expr_t>& The base expression representing the record.
     */
    [[nodiscard]] auto base() -> safe_ptr<expr_t>& { return base_; }

    /**
     * @brief Get the field name token.
     * @return const token_t& The field name token.
     */
    [[nodiscard]] auto field_name() const -> const token_t& { return field_name_; }
    /**
     * @brief Get the field name token.
     * @return token_t& The field name token.
     */
    [[nodiscard]] auto field_name() -> token_t& { return field_name_; }

private:
    safe_ptr<expr_t> base_;
    token_t          field_name_;
};

// TODO: Closure expression

/**
 * @class call_t
 * @brief represents a function call expression.
 */
class call_t {
public:
    call_t() = delete;

    call_t(const call_t&)     = delete;
    call_t(call_t&&) noexcept = default;
    ~call_t()                 = default;

    auto operator=(const call_t&) -> call_t&     = delete;
    auto operator=(call_t&&) noexcept -> call_t& = default;

    /**
     * @brief Struct representing a named argument in the function call.
     */
    struct named_arg_t;

    /**
     * @brief Constructs a function call expression with the given callee and arguments.
     *
     * @param callee The expression representing the function being called.
     * @param positional_args The positional arguments of the function call.
     * @param named_args The named arguments of the function call.
     */
    call_t(safe_ptr<expr_t>&& callee, std::vector<expr_t>&& positional_args, std::vector<named_arg_t>&& named_args);

    /**
     * @brief Get the callee expression of the function call.
     * @return const safe_ptr<expr_t>& The callee expression of the function call.
     */
    [[nodiscard]] auto callee() const -> const safe_ptr<expr_t>& { return callee_; }
    /**
     * @brief Get the callee expression of the function call.
     * @return safe_ptr<expr_t>& The callee expression of the function call.
     */
    [[nodiscard]] auto callee() -> safe_ptr<expr_t>& { return callee_; }

    /**
     * @brief Get the positional arguments of the function call.
     * @return const std::vector<expr_t>& The positional arguments of the function call.
     */
    [[nodiscard]] auto positional_args() const -> const std::vector<expr_t>& { return positional_args_; }
    /**
     * @brief Get the positional arguments of the function call.
     * @return std::vector<expr_t>& The positional arguments of the function call.
     */
    [[nodiscard]] auto positional_args() -> std::vector<expr_t>& { return positional_args_; }

    /**
     * @brief Get the named arguments of the function call.
     * @return const std::vector<named_arg_t>& The named arguments of the function call.
     */
    [[nodiscard]] auto named_args() const -> const std::vector<named_arg_t>& { return named_args_; }
    /**
     * @brief Get the named arguments of the function call.
     * @return std::vector<named_arg_t>& The named arguments of the function call.
     */
    [[nodiscard]] auto named_args() -> std::vector<named_arg_t>& { return named_args_; }

private:
    safe_ptr<expr_t>         callee_;
    std::vector<expr_t>      positional_args_;
    std::vector<named_arg_t> named_args_;
};

/**
 * @class if_t
 * @brief represents an if expression.
 */
class if_t {
public:
    if_t() = delete;

    if_t(const if_t&)     = delete;
    if_t(if_t&&) noexcept = default;
    ~if_t()               = default;

    auto operator=(const if_t&) -> if_t&     = delete;
    auto operator=(if_t&&) noexcept -> if_t& = default;

    /**
     * @brief Constructs an if expression with the given condition, then branch, and an else branch.
     *
     * @param condition The condition expression.
     * @param then_branch The expression to execute if the condition is true.
     * @param else_branch The expression to execute if the condition is false.
     */
    if_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& then_branch, safe_ptr<expr_t>&& else_branch);

    /**
     * @brief Constructs an if expression with the given condition and then branch (no else branch).
     *
     * @param condition The condition expression.
     * @param then_branch The expression to execute if the condition is true.
     * @remark This represents an if expression without an else branch, the else branch is implicitly null.
     */
    if_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& then_branch);

    /**
     * @brief Get the condition expression of the if expression.
     * @return const safe_ptr<expr_t>& The condition expression of the if expression.
     */
    [[nodiscard]] auto condition() const -> const safe_ptr<expr_t>& { return condition_; }
    /**
     * @brief Get the condition expression of the if expression.
     * @return safe_ptr<expr_t>& The condition expression of the if expression.
     */
    [[nodiscard]] auto condition() -> safe_ptr<expr_t>& { return condition_; }

    /**
     * @brief Get the then branch expression of the if expression.
     * @return const safe_ptr<expr_t>& The then branch expression of the if expression.
     */
    [[nodiscard]] auto then_branch() const -> const safe_ptr<expr_t>& { return then_branch_; }
    /**
     * @brief Get the then branch expression of the if expression.
     * @return safe_ptr<expr_t>& The then branch expression of the if expression.
     */
    [[nodiscard]] auto then_branch() -> safe_ptr<expr_t>& { return then_branch_; }

    /**
     * @brief Get the else branch expression of the if expression.
     * @return const nullable_ptr<expr_t>& The else branch expression of the if expression.
     */
    [[nodiscard]] auto else_branch() const -> const nullable_ptr<expr_t>& { return else_branch_; }
    /**
     * @brief Get the else branch expression of the if expression.
     * @return nullable_ptr<expr_t>& The else branch expression of the if expression.
     */
    [[nodiscard]] auto else_branch() -> nullable_ptr<expr_t>& { return else_branch_; }

private:
    safe_ptr<expr_t>     condition_;
    safe_ptr<expr_t>     then_branch_;
    nullable_ptr<expr_t> else_branch_;
};

// TODO: For expression

/**
 * @class while_t
 * @brief represents a while loop expression.
 */
class while_t {
public:
    while_t() = delete;

    while_t(const while_t&)     = delete;
    while_t(while_t&&) noexcept = default;
    ~while_t()                  = default;

    auto operator=(const while_t&) -> while_t&     = delete;
    auto operator=(while_t&&) noexcept -> while_t& = default;

    /**
     * @brief Constructs a while loop expression with the given condition and body.
     *
     * @param condition The condition expression.
     * @param body The body expression to execute while the condition is true.
     */
    while_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& body);

    /**
     * @brief Get the condition expression of the while loop.
     * @return const safe_ptr<expr_t>& The condition expression of the while loop.
     */
    [[nodiscard]] auto condition() const -> const safe_ptr<expr_t>& { return condition_; }
    /**
     * @brief Get the condition expression of the while loop.
     * @return safe_ptr<expr_t>& The condition expression of the while loop.
     */
    [[nodiscard]] auto condition() -> safe_ptr<expr_t>& { return condition_; }

    /**
     * @brief Get the body expression of the while loop.
     * @return const safe_ptr<expr_t>& The body expression of the while loop.
     */
    [[nodiscard]] auto body() const -> const safe_ptr<expr_t>& { return body_; }
    /**
     * @brief Get the body expression of the while loop.
     * @return safe_ptr<expr_t>& The body expression of the while loop.
     */
    [[nodiscard]] auto body() -> safe_ptr<expr_t>& { return body_; }

private:
    safe_ptr<expr_t> condition_;
    safe_ptr<expr_t> body_;
};

/**
 * @class block_t
 * @brief represents a block expression.
 */
class block_t {
public:
    block_t() = delete;

    block_t(const block_t&)     = delete;
    block_t(block_t&&) noexcept = default;
    ~block_t();

    auto operator=(const block_t&) -> block_t&     = delete;
    auto operator=(block_t&&) noexcept -> block_t& = default;

    /**
     * @brief Constructs a block expression with the given statements and optional return expression.
     *
     * @param statements The statements within the block.
     * @param return_expr The optional return expression of the block.
     */
    block_t(std::vector<stmt::stmt_t>&& statements, safe_ptr<expr_t>&& return_expr);

    /**
     * @brief Get the statements within the block expression.
     * @return const std::vector<stmt::stmt_t>& The statements within the block expression.
     */
    [[nodiscard]] auto statements() const -> const std::vector<stmt::stmt_t>& { return statements_; }

    /**
     * @brief Get the statements within the block expression.
     * @return std::vector<stmt::stmt_t>& The statements within the block expression.
     */
    [[nodiscard]] auto statements() -> std::vector<stmt::stmt_t>& { return statements_; }

    /**
     * @brief Get the return expression of the block expression.
     * @return const safe_ptr<expr_t>& The return expression of the block expression.
     */
    [[nodiscard]] auto return_expr() const -> const safe_ptr<expr_t>& { return return_expr_; }
    /**
     * @brief Get the return expression of the block expression.
     * @return safe_ptr<expr_t>& The return expression of the block expression.
     */
    [[nodiscard]] auto return_expr() -> safe_ptr<expr_t>& { return return_expr_; }

private:
    std::vector<stmt::stmt_t> statements_;
    safe_ptr<expr_t>          return_expr_;
};

/**
 * @class grouping_t
 * @brief represents a grouping expression.
 */
class grouping_t {
public:
    grouping_t() = delete;

    grouping_t(const grouping_t&)     = delete;
    grouping_t(grouping_t&&) noexcept = default;
    ~grouping_t()                     = default;

    auto operator=(const grouping_t&) -> grouping_t&     = delete;
    auto operator=(grouping_t&&) noexcept -> grouping_t& = default;

    /**
     * @brief Constructs a grouping expression with the given expression.
     * @param expression The expression within the grouping.
     */
    explicit grouping_t(safe_ptr<expr_t>&& expression);

    /**
     * @brief Get the expression within the grouping.
     * @return const safe_ptr<expr_t>& The expression within the grouping.
     */
    [[nodiscard]] auto expression() const -> const safe_ptr<expr_t>& { return expression_; }
    /**
     * @brief Get the expression within the grouping.
     * @return safe_ptr<expr_t>& The expression within the grouping.
     */
    [[nodiscard]] auto expression() -> safe_ptr<expr_t>& { return expression_; }

private:
    safe_ptr<expr_t> expression_;
};

/**
 * @class error_t
 * @brief represents an error in an expression.
 */
class error_t {
public:
    error_t() = default;

    error_t(const error_t&)     = default;
    error_t(error_t&&) noexcept = default;
    ~error_t()                  = default;

    auto operator=(const error_t&) -> error_t&     = default;
    auto operator=(error_t&&) noexcept -> error_t& = default;
};

/**
 * @class expr_t
 * @brief represents an expression.
 */
class expr_t
    : public node_t<literal_t,
                    identifier_t,
                    binary_t,
                    unary_t,
                    array_t,
                    is_t,
                    cast_t,
                    tuple_t,
                    record_t,
                    index_t,
                    field_t,
                    // closure_t,
                    call_t,
                    if_t,
                    // for_t,
                    while_t,
                    block_t,
                    grouping_t,
                    error_t> {
public:
    using node_t::node_t;
};

struct record_t::field_t {
    token_t name;
    expr_t  value;
};

struct call_t::named_arg_t {
    token_t name;
    expr_t  value;
};

} // namespace loxmocha::expr
