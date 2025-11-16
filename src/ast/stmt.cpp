#include "loxmocha/ast/stmt.hpp"

#include "loxmocha/ast/expr.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <utility>

namespace loxmocha::stmt {

expr_t::expr_t(safe_ptr<expr::expr_t>&& expr) : expr_(std::move(expr)) {}

} // namespace loxmocha::stmt
