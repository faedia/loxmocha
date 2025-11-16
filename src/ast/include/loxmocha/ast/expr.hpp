#pragma once

#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

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

class literal_t {
public:
    literal_t() = delete;

    literal_t(const literal_t&)     = default;
    literal_t(literal_t&&) noexcept = default;
    ~literal_t()                    = default;

    auto operator=(const literal_t&) -> literal_t&     = default;
    auto operator=(literal_t&&) noexcept -> literal_t& = default;

    explicit literal_t(const token_t& value) : value_(value) {}

    [[nodiscard]] auto value() const -> const token_t& { return value_; }
    [[nodiscard]] auto value() -> token_t& { return value_; }

private:
    token_t value_;
};

class identifier_t {
public:
    identifier_t() = delete;

    identifier_t(const identifier_t&)     = default;
    identifier_t(identifier_t&&) noexcept = default;
    ~identifier_t()                       = default;

    auto operator=(const identifier_t&) -> identifier_t&     = default;
    auto operator=(identifier_t&&) noexcept -> identifier_t& = default;

    explicit identifier_t(const token_t& name) : name_(name) {}

    [[nodiscard]] auto name() const -> const token_t& { return name_; }
    [[nodiscard]] auto name() -> token_t& { return name_; }

private:
    token_t name_;
};

class binary_t {
public:
    binary_t() = delete;

    binary_t(const binary_t&)     = delete;
    binary_t(binary_t&&) noexcept = default;
    ~binary_t()                   = default;

    auto operator=(const binary_t&) -> binary_t&     = delete;
    auto operator=(binary_t&&) noexcept -> binary_t& = default;

    inline binary_t(const token_t& op, safe_ptr<expr_t>&& left, safe_ptr<expr_t>&& right);

    [[nodiscard]] auto op() const -> const token_t& { return op_; }
    [[nodiscard]] auto op() -> token_t& { return op_; }

    [[nodiscard]] auto left() const -> const safe_ptr<expr_t>& { return left_; }
    [[nodiscard]] auto left() -> safe_ptr<expr_t>& { return left_; }

    [[nodiscard]] auto right() const -> const safe_ptr<expr_t>& { return right_; }
    [[nodiscard]] auto right() -> safe_ptr<expr_t>& { return right_; }

private:
    token_t          op_;
    safe_ptr<expr_t> left_;
    safe_ptr<expr_t> right_;
};

class unary_t {
public:
    unary_t() = delete;

    unary_t(const unary_t&)     = delete;
    unary_t(unary_t&&) noexcept = default;
    ~unary_t()                  = default;

    auto operator=(const unary_t&) -> unary_t&     = delete;
    auto operator=(unary_t&&) noexcept -> unary_t& = default;

    inline unary_t(const token_t& op, safe_ptr<expr_t>&& operand);

    [[nodiscard]] auto op() const -> const token_t& { return op_; }
    [[nodiscard]] auto op() -> token_t& { return op_; }

    [[nodiscard]] auto operand() const -> const safe_ptr<expr_t>& { return operand_; }
    [[nodiscard]] auto operand() -> safe_ptr<expr_t>& { return operand_; }

private:
    token_t          op_;
    safe_ptr<expr_t> operand_;
};

class is_t {
public:
    is_t() = delete;

    is_t(const is_t&)     = delete;
    is_t(is_t&&) noexcept = default;
    ~is_t()               = default;

    auto operator=(const is_t&) -> is_t&     = delete;
    auto operator=(is_t&&) noexcept -> is_t& = default;

    inline is_t(safe_ptr<expr_t>&& expr, safe_ptr<type::type_t>&& type, const token_t& is_token);

    [[nodiscard]] auto expr() const -> const safe_ptr<expr_t>& { return expr_; }
    [[nodiscard]] auto expr() -> safe_ptr<expr_t>& { return expr_; }

    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    safe_ptr<expr_t>       expr_;
    safe_ptr<type::type_t> type_;
};

class cast_t {
public:
    cast_t() = delete;

    cast_t(const cast_t&)     = delete;
    cast_t(cast_t&&) noexcept = default;
    ~cast_t()                 = default;

    auto operator=(const cast_t&) -> cast_t&     = delete;
    auto operator=(cast_t&&) noexcept -> cast_t& = default;

    inline cast_t(safe_ptr<expr_t>&& expr, safe_ptr<type::type_t>&& type, const token_t& as_token);

    [[nodiscard]] auto expr() const -> const safe_ptr<expr_t>& { return expr_; }
    [[nodiscard]] auto expr() -> safe_ptr<expr_t>& { return expr_; }

    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    safe_ptr<expr_t>       expr_;
    safe_ptr<type::type_t> type_;
};

class array_t {
public:
    array_t() = delete;

    array_t(const array_t&)     = delete;
    array_t(array_t&&) noexcept = default;
    ~array_t()                  = default;

    auto operator=(const array_t&) -> array_t&     = default;
    auto operator=(array_t&&) noexcept -> array_t& = default;

    inline array_t(std::vector<expr_t>&& elements, const token_t& open_bracket, const token_t& close_bracket);

    [[nodiscard]] auto elements() const -> const std::vector<expr_t>& { return elements_; }
    [[nodiscard]] auto elements() -> std::vector<expr_t>& { return elements_; }

    [[nodiscard]] auto open_bracket() const -> const token_t& { return open_bracket_; }
    [[nodiscard]] auto open_bracket() -> token_t& { return open_bracket_; }

    [[nodiscard]] auto close_bracket() const -> const token_t& { return close_bracket_; }
    [[nodiscard]] auto close_bracket() -> token_t& { return close_bracket_; }

private:
    std::vector<expr_t> elements_;
    token_t             open_bracket_;
    token_t             close_bracket_;
};

class tuple_t {
public:
    tuple_t() = delete;

    tuple_t(const tuple_t&)     = delete;
    tuple_t(tuple_t&&) noexcept = default;
    ~tuple_t()                  = default;

    auto operator=(const tuple_t&) -> tuple_t&     = delete;
    auto operator=(tuple_t&&) noexcept -> tuple_t& = default;

    inline tuple_t(std::vector<expr_t>&& elements, const token_t& open_paren, const token_t& close_paren);

    [[nodiscard]] auto elements() const -> const std::vector<expr_t>& { return elements_; }
    [[nodiscard]] auto elements() -> std::vector<expr_t>& { return elements_; }

    [[nodiscard]] auto open_paren() const -> const token_t& { return open_paren_; }
    [[nodiscard]] auto open_paren() -> token_t& { return open_paren_; }

    [[nodiscard]] auto close_paren() const -> const token_t& { return close_paren_; }
    [[nodiscard]] auto close_paren() -> token_t& { return close_paren_; }

private:
    std::vector<expr_t> elements_;
    token_t             open_paren_;
    token_t             close_paren_;
};

class record_t {
public:
    record_t() = delete;

    record_t(const record_t&)     = delete;
    record_t(record_t&&) noexcept = default;
    ~record_t()                   = default;

    auto operator=(const record_t&) -> record_t&     = delete;
    auto operator=(record_t&&) noexcept -> record_t& = default;

    inline record_t(std::vector<std::pair<token_t, expr_t>>&& fields,
                    const token_t&                            open_brace,
                    const token_t&                            close_brace);

    [[nodiscard]] auto fields() const -> const std::vector<std::pair<token_t, expr_t>>& { return fields_; }
    [[nodiscard]] auto fields() -> std::vector<std::pair<token_t, expr_t>>& { return fields_; }

    [[nodiscard]] auto open_brace() const -> const token_t& { return open_brace_; }
    [[nodiscard]] auto open_brace() -> token_t& { return open_brace_; }

    [[nodiscard]] auto close_brace() const -> const token_t& { return close_brace_; }
    [[nodiscard]] auto close_brace() -> token_t& { return close_brace_; }

private:
    std::vector<std::pair<token_t, expr_t>> fields_;
    token_t                                 open_brace_;
    token_t                                 close_brace_;
};

class index_t {
public:
    index_t() = delete;

    index_t(const index_t&)     = delete;
    index_t(index_t&&) noexcept = default;
    ~index_t()                  = default;

    auto operator=(const index_t&) -> index_t&     = delete;
    auto operator=(index_t&&) noexcept -> index_t& = default;

    inline index_t(safe_ptr<expr_t>&& base, safe_ptr<expr_t>&& index);

    [[nodiscard]] auto base() const -> const safe_ptr<expr_t>& { return base_; }
    [[nodiscard]] auto base() -> safe_ptr<expr_t>& { return base_; }

    [[nodiscard]] auto index() const -> const safe_ptr<expr_t>& { return index_; }
    [[nodiscard]] auto index() -> safe_ptr<expr_t>& { return index_; }

private:
    safe_ptr<expr_t> base_;
    safe_ptr<expr_t> index_;
};

class field_t {
public:
    field_t() = delete;

    field_t(const field_t&)     = delete;
    field_t(field_t&&) noexcept = default;
    ~field_t()                  = default;

    auto operator=(const field_t&) -> field_t&     = delete;
    auto operator=(field_t&&) noexcept -> field_t& = default;

    inline field_t(safe_ptr<expr_t>&& base, const token_t& field_name);

    [[nodiscard]] auto base() const -> const safe_ptr<expr_t>& { return base_; }
    [[nodiscard]] auto base() -> safe_ptr<expr_t>& { return base_; }

    [[nodiscard]] auto field_name() const -> const token_t& { return field_name_; }
    [[nodiscard]] auto field_name() -> token_t& { return field_name_; }

private:
    safe_ptr<expr_t> base_;
    token_t          field_name_;
};

// TODO: Closure expression

class call_t {
public:
    call_t() = delete;

    call_t(const call_t&)     = delete;
    call_t(call_t&&) noexcept = default;
    ~call_t()                 = default;

    auto operator=(const call_t&) -> call_t&     = delete;
    auto operator=(call_t&&) noexcept -> call_t& = default;

    inline call_t(safe_ptr<expr_t>&&                        callee,
                  std::vector<expr_t>&&                     positional_args,
                  std::vector<std::pair<token_t, expr_t>>&& named_args);

    [[nodiscard]] auto callee() const -> const safe_ptr<expr_t>& { return callee_; }
    [[nodiscard]] auto callee() -> safe_ptr<expr_t>& { return callee_; }

    [[nodiscard]] auto positional_args() const -> const std::vector<expr_t>& { return positional_args_; }
    [[nodiscard]] auto positional_args() -> std::vector<expr_t>& { return positional_args_; }

    [[nodiscard]] auto named_args() const -> const std::vector<std::pair<token_t, expr_t>>& { return named_args_; }
    [[nodiscard]] auto named_args() -> std::vector<std::pair<token_t, expr_t>>& { return named_args_; }

private:
    safe_ptr<expr_t>                        callee_;
    std::vector<expr_t>                     positional_args_;
    std::vector<std::pair<token_t, expr_t>> named_args_;
};

class if_t {
public:
    if_t() = delete;

    if_t(const if_t&)     = delete;
    if_t(if_t&&) noexcept = default;
    ~if_t()               = default;

    auto operator=(const if_t&) -> if_t&     = delete;
    auto operator=(if_t&&) noexcept -> if_t& = default;

    inline if_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& then_branch, nullable_ptr<expr_t>&& else_branch);

    inline if_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& then_branch);

    [[nodiscard]] auto condition() const -> const safe_ptr<expr_t>& { return condition_; }
    [[nodiscard]] auto condition() -> safe_ptr<expr_t>& { return condition_; }

    [[nodiscard]] auto then_branch() const -> const safe_ptr<expr_t>& { return then_branch_; }
    [[nodiscard]] auto then_branch() -> safe_ptr<expr_t>& { return then_branch_; }

    [[nodiscard]] auto else_branch() const -> const nullable_ptr<expr_t>& { return else_branch_; }
    [[nodiscard]] auto else_branch() -> nullable_ptr<expr_t>& { return else_branch_; }

private:
    safe_ptr<expr_t>     condition_;
    safe_ptr<expr_t>     then_branch_;
    nullable_ptr<expr_t> else_branch_;
};

// TODO: For expression

class while_t {
public:
    while_t() = delete;

    while_t(const while_t&)     = delete;
    while_t(while_t&&) noexcept = default;
    ~while_t()                  = default;

    auto operator=(const while_t&) -> while_t&     = delete;
    auto operator=(while_t&&) noexcept -> while_t& = default;

    inline while_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& body);

    [[nodiscard]] auto condition() const -> const safe_ptr<expr_t>& { return condition_; }
    [[nodiscard]] auto condition() -> safe_ptr<expr_t>& { return condition_; }

    [[nodiscard]] auto body() const -> const safe_ptr<expr_t>& { return body_; }
    [[nodiscard]] auto body() -> safe_ptr<expr_t>& { return body_; }

private:
    safe_ptr<expr_t> condition_;
    safe_ptr<expr_t> body_;
};

class block_t {
public:
    block_t() = delete;

    block_t(const block_t&)     = delete;
    block_t(block_t&&) noexcept = default;
    ~block_t();

    auto operator=(const block_t&) -> block_t&     = delete;
    auto operator=(block_t&&) noexcept -> block_t& = default;

    block_t(std::vector<stmt::stmt_t>&& statements, safe_ptr<expr_t>&& return_expr);

    [[nodiscard]] auto statements() const -> const std::vector<stmt::stmt_t>& { return statements_; }
    [[nodiscard]] auto statements() -> std::vector<stmt::stmt_t>& { return statements_; }

    [[nodiscard]] auto return_expr() const -> const safe_ptr<expr_t>& { return return_expr_; }
    [[nodiscard]] auto return_expr() -> safe_ptr<expr_t>& { return return_expr_; }

private:
    std::vector<stmt::stmt_t> statements_;
    safe_ptr<expr_t>          return_expr_;
};

class grouping_t {
public:
    grouping_t() = delete;

    grouping_t(const grouping_t&)     = delete;
    grouping_t(grouping_t&&) noexcept = default;
    ~grouping_t()                     = default;

    auto operator=(const grouping_t&) -> grouping_t&     = delete;
    auto operator=(grouping_t&&) noexcept -> grouping_t& = default;

    inline explicit grouping_t(safe_ptr<expr_t>&& expression);

    [[nodiscard]] auto expression() const -> const safe_ptr<expr_t>& { return expression_; }
    [[nodiscard]] auto expression() -> safe_ptr<expr_t>& { return expression_; }

private:
    safe_ptr<expr_t> expression_;
};

class error_t {
public:
    error_t() = default;

    error_t(const error_t&)     = default;
    error_t(error_t&&) noexcept = default;
    ~error_t()                  = default;

    auto operator=(const error_t&) -> error_t&     = default;
    auto operator=(error_t&&) noexcept -> error_t& = default;
};

class expr_t {
public:
    expr_t() = delete;

    expr_t(const expr_t&)     = delete;
    expr_t(expr_t&&) noexcept = default;
    ~expr_t()                 = default;

    auto operator=(const expr_t&) -> expr_t&     = delete;
    auto operator=(expr_t&&) noexcept -> expr_t& = default;
    template<typename T>
    auto operator=(T&& value) noexcept -> expr_t&
    {
        expr_ = std::forward<T>(value);
        return *this;
    }

    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    expr_t(T&& value) noexcept : expr_(std::forward<T>(value))
    {
    }

    template<typename T, typename... Args>
    // NOLINTNEXTLINE(readability-named-parameter,hicpp-named-parameter)
    explicit expr_t(std::in_place_type_t<T>, Args&&... args) : expr_(std::in_place_type<T>, std::forward<Args>(args)...)
    {
    }

    template<typename T>
    [[nodiscard]] auto is() const -> bool
    {
        return std::holds_alternative<T>(expr_);
    }

    template<typename T>
    [[nodiscard]] auto as() -> T&
    {
        return std::get<T>(expr_);
    }

    template<typename T>
    [[nodiscard]] auto as() const -> const T&
    {
        return std::get<T>(expr_);
    }

    template<typename Visitor, typename... Args>
    auto visit(Visitor&& visitor, Args&&... args) const
    {
        return std::visit([&visitor, &args...](auto&& arg) { return visitor(arg, std::forward<Args>(args)...); },
                          expr_);
    }

private:
    std::variant<literal_t,
                 identifier_t,
                 binary_t,
                 unary_t,
                 array_t,
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
                 error_t>
        expr_;
};

inline binary_t::binary_t(const token_t& op, safe_ptr<expr_t>&& left, safe_ptr<expr_t>&& right)
    : op_(op), left_(std::move(left)), right_(std::move(right))
{
}

inline unary_t::unary_t(const token_t& op, safe_ptr<expr_t>&& operand) : op_(op), operand_(std::move(operand)) {}

inline array_t::array_t(std::vector<expr_t>&& elements, const token_t& open_bracket, const token_t& close_bracket)
    : elements_(std::move(elements)), open_bracket_(open_bracket), close_bracket_(close_bracket)
{
}

inline tuple_t::tuple_t(std::vector<expr_t>&& elements, const token_t& open_paren, const token_t& close_paren)
    : elements_(std::move(elements)), open_paren_(open_paren), close_paren_(close_paren)
{
}

inline record_t::record_t(std::vector<std::pair<token_t, expr_t>>&& fields,
                          const token_t&                            open_brace,
                          const token_t&                            close_brace)
    : fields_(std::move(fields)), open_brace_(open_brace), close_brace_(close_brace)
{
}

inline index_t::index_t(safe_ptr<expr_t>&& base, safe_ptr<expr_t>&& index)
    : base_(std::move(base)), index_(std::move(index))
{
}

inline field_t::field_t(safe_ptr<expr_t>&& base, const token_t& field_name)
    : base_(std::move(base)), field_name_(field_name)
{
}

inline call_t::call_t(safe_ptr<expr_t>&&                        callee,
                      std::vector<expr_t>&&                     positional_args,
                      std::vector<std::pair<token_t, expr_t>>&& named_args)
    : callee_(std::move(callee)), positional_args_(std::move(positional_args)), named_args_(std::move(named_args))
{
}

inline if_t::if_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& then_branch, nullable_ptr<expr_t>&& else_branch)
    : condition_(std::move(condition)), then_branch_(std::move(then_branch)), else_branch_(std::move(else_branch))
{
}

inline if_t::if_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& then_branch)
    : condition_(std::move(condition)), then_branch_(std::move(then_branch)), else_branch_(nullptr)
{
}

inline while_t::while_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& body)
    : condition_(std::move(condition)), body_(std::move(body))
{
}

inline grouping_t::grouping_t(safe_ptr<expr_t>&& expression) : expression_(std::move(expression)) {}

} // namespace loxmocha::expr
