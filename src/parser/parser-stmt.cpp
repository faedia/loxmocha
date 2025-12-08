#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <utility>

namespace loxmocha::internal {

auto parser_t::parse_stmt() -> parser_result_t<stmt::stmt_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<stmt::stmt_t>{parse_stmt_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_stmt_internal() -> stmt::stmt_t
{
    if (auto token = lexer_.peek_token(); token && is_decl_start_token(*token)) {
        return stmt::decl_t{safe_ptr<decl::decl_t>::make(parse_decl_internal())};
    }

    auto expr = parse_expr_internal();

    if (auto equal = lexer_.peek_token(); equal && match<token_t::kind_e::p_equal>(*equal)) {
        lexer_.consume_token();
        auto value_expr = parse_expr_internal();
        return stmt::assign_t{safe_ptr<expr::expr_t>::make(std::move(expr)),
                              safe_ptr<expr::expr_t>::make(std::move(value_expr))};
    }
    return stmt::expr_t{safe_ptr<expr::expr_t>::make(std::move(expr))};
}

} // namespace loxmocha::internal
