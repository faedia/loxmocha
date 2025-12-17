#include "loxmocha/ast/parser.hpp"

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "parser-internal.hpp"

namespace loxmocha {

auto parse_decl(lexer_t& lexer) -> parser_result_t<decl::decl_t> { return internal::parser_t{lexer}.parse_decl(); }

auto parse_module([[maybe_unused]] lexer_t& lexer) -> parser_result_t<module::module_t>
{
    return internal::parser_t{lexer}.parse_module();
}

auto parse_stmt(lexer_t& lexer) -> parser_result_t<stmt::stmt_t> { return internal::parser_t{lexer}.parse_stmt(); }

auto parse_expr(lexer_t& lexer) -> parser_result_t<expr::expr_t> { return internal::parser_t{lexer}.parse_expr(); }

auto parse_type(lexer_t& lexer) -> parser_result_t<type::type_t> { return internal::parser_t{lexer}.parse_type(); }

auto parse_pattern(lexer_t& lexer) -> parser_result_t<pattern::pattern_t>
{
    return internal::parser_t{lexer}.parse_pattern();
}

} // namespace loxmocha
