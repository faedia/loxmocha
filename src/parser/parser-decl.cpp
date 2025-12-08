#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "parser-internal.hpp"

#include <utility>

namespace loxmocha::internal {

auto parser_t::parse_decl() -> parser_result_t<decl::decl_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<decl::decl_t>{parse_decl_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_decl_internal() -> decl::decl_t
{
    has_error_ = true;
    return decl::error_t{"Not implemented"};
}

auto parser_t::is_decl_start_token(const token_t& token) -> bool
{
    switch (token.kind()) {
    case token_t::kind_e::k_fun:
    case token_t::kind_e::k_let:
    case token_t::kind_e::k_type:
    case token_t::kind_e::k_var: return true;
    default: return false;
    }
}

} // namespace loxmocha::internal
