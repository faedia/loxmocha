#include "loxmocha/ast/decl.hpp"

#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <utility>
#include <vector>

namespace loxmocha::decl {

type_t::type_t(token_t identifier, safe_ptr<type::type_t>&& type) : identifier_(identifier), type_(std::move(type)) {}

function_t::function_t(token_t                                identifier,
                       std::vector<function_t::parameter_t>&& parameters,
                       safe_ptr<type::type_t>&&               return_type,
                       safe_ptr<expr::expr_t>&&               body)
    : identifier_(identifier)
    , parameters_(std::move(parameters))
    , return_type_(std::move(return_type))
    , body_(std::move(body))
{
}

variable_t::variable_t(token_t identifier, safe_ptr<type::type_t>&& type)
    : identifier_(identifier), type_(std::move(type))
{
}

} // namespace loxmocha::decl