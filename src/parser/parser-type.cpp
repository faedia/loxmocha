#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "parser-internal.hpp"
#include "loxmocha/ast/parser.hpp"

#include <utility>

namespace loxmocha::internal {

auto parser_t::parse_type() -> parser_result_t<type::type_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<type::type_t>{parse_type_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_type_internal() -> type::type_t
{
    diagnostics_.emplace_back("Type parsing not implemented");
    return type::identifier_t{token_t::k_identifier("MyType")};
}

} // namespace loxmocha::internal
