#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "parser-internal.hpp"

namespace loxmocha::internal {

auto parser_t::parse_type_internal() -> type::type_t
{
    diagnostics_.emplace_back("Type parsing not implemented");
    return type::identifier_t{token_t::k_identifier("MyType")};
}

} // namespace loxmocha::internal
