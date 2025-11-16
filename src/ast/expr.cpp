#include "loxmocha/ast/expr.hpp"

#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <utility>
#include <vector>

namespace loxmocha::expr {

block_t::~block_t() = default;

block_t::block_t(std::vector<stmt::stmt_t>&& statements, safe_ptr<expr_t>&& return_expr)
    : statements_(std::move(statements)), return_expr_(std::move(return_expr))
{
}

} // namespace loxmocha::expr
