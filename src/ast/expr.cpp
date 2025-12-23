#include "loxmocha/ast/expr.hpp"

#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <utility>
#include <vector>

namespace loxmocha::ast::expr {

using namespace loxmocha::lexer;

binary_t::binary_t(const token_t& op, safe_ptr<expr_t>&& left, safe_ptr<expr_t>&& right)
    : op_(op), left_(std::move(left)), right_(std::move(right))
{
}

unary_t::unary_t(const token_t& op, safe_ptr<expr_t>&& operand) : op_(op), operand_(std::move(operand)) {}

is_t::is_t(safe_ptr<expr_t>&& expr, safe_ptr<pattern::pattern_t>&& pattern)
    : expr_(std::move(expr)), pattern_(std::move(pattern))
{
}
is_t::~is_t() = default;

cast_t::cast_t(safe_ptr<expr_t>&& expr, safe_ptr<type::type_t>&& type) : expr_(std::move(expr)), type_(std::move(type))
{
}
cast_t::~cast_t() = default;

array_t::array_t(std::vector<expr_t>&& elements) : elements_(std::move(elements)) {}

tuple_t::tuple_t(std::vector<expr_t>&& elements) : elements_(std::move(elements)) {}

record_t::record_t(std::vector<field_t>&& fields) : fields_(std::move(fields)) {}

index_t::index_t(safe_ptr<expr_t>&& base, safe_ptr<expr_t>&& index) : base_(std::move(base)), index_(std::move(index))
{
}

field_t::field_t(safe_ptr<expr_t>&& base, const token_t& field_name) : base_(std::move(base)), field_name_(field_name)
{
}

call_t::call_t(safe_ptr<expr_t>&& callee, std::vector<expr_t>&& positional_args, std::vector<named_arg_t>&& named_args)
    : callee_(std::move(callee)), positional_args_(std::move(positional_args)), named_args_(std::move(named_args))
{
}

if_t::if_t(std::vector<conditional_branch_t>&& branches, safe_ptr<expr_t>&& else_branch)
    : conditional_branches_(std::move(branches)), else_branch_(std::move(else_branch).to_nullable())
{
}

if_t::if_t(std::vector<conditional_branch_t>&& branches)
    : conditional_branches_(std::move(branches)), else_branch_(nullptr)
{
}

while_t::while_t(safe_ptr<expr_t>&& condition, safe_ptr<expr_t>&& body)
    : condition_(std::move(condition)), body_(std::move(body))
{
}

block_t::~block_t() = default;

block_t::block_t(std::vector<stmt::stmt_t>&& statements, safe_ptr<expr_t>&& return_expr)
    : statements_(std::move(statements)), return_expr_(std::move(return_expr))
{
}

grouping_t::grouping_t(safe_ptr<expr_t>&& expression) : expression_(std::move(expression)) {}

} // namespace loxmocha::ast::expr
