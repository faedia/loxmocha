#include "loxmocha/ast/parser.hpp"

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <string>
#include <utility>
#include <vector>

namespace loxmocha {

auto parse_decl([[maybe_unused]] lexer_t& lexer) -> parser_result_t<decl::decl_t>
{
    return parser_result_t<decl::decl_t>{decl::error_t{"Not implemented"}, true, {"Not implemented"}};
}

auto parse_module([[maybe_unused]] lexer_t& lexer) -> parser_result_t<module::module_t>
{
    return parser_result_t<module::module_t>{module::module_t{}, true, {"Not implemented"}};
}

auto parse_stmt([[maybe_unused]] lexer_t& lexer) -> parser_result_t<stmt::stmt_t>
{
    stmt::stmt_t stmt = stmt::expr_t{safe_ptr<expr::expr_t>::make(expr::identifier_t(token_t::l_integer("42")))};
    return parser_result_t<stmt::stmt_t>{std::move(stmt), false, {}};
}

auto parse_expr(lexer_t& lexer) -> parser_result_t<expr::expr_t> { return internal::parser_t{lexer}.parse_expr(); }

auto parse_type([[maybe_unused]] lexer_t& lexer) -> parser_result_t<type::type_t>
{
    return parser_result_t<type::type_t>{type::identifier_t(token_t::k_identifier("int")), false, {}};
}

auto parse_pattern([[maybe_unused]] lexer_t& lexer) -> parser_result_t<pattern::pattern_t>
{
    return parser_result_t<pattern::pattern_t>{pattern::identifier_t{token_t::k_identifier("my_var")}, false, {}};
}

} // namespace loxmocha
