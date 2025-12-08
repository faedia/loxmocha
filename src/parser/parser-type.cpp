#include "loxmocha/ast/type.hpp"
#include "parser-internal.hpp"
#include "loxmocha/ast/token.hpp"

namespace loxmocha::internal {

auto parser_t::parse_type_internal() -> type::type_t
{
    diagnostics_.emplace_back("Type parsing not implemented");
    has_error_ = true;
    return type::identifier_t{token_t::k_identifier("error")};
}

} // namespace loxmocha::internal
