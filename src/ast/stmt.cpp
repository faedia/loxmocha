#include "loxmocha/ast/stmt.hpp"

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <utility>

namespace loxmocha::ast::stmt {

expr_t::expr_t(safe_ptr<expr::expr_t>&& expr) : expr_(std::move(expr)) {}
expr_t::~expr_t() = default;

assign_t::assign_t(safe_ptr<expr::expr_t>&& target, safe_ptr<expr::expr_t>&& value)
    : target_(std::move(target)), value_(std::move(value))
{
}
assign_t::~assign_t() = default;

decl_t::decl_t(safe_ptr<decl::decl_t>&& declaration) : declaration_(std::move(declaration)) {}
decl_t::~decl_t() = default;

} // namespace loxmocha::ast::stmt
