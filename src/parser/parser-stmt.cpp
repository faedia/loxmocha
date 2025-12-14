#include "loxmocha/ast/base.hpp"
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
    // If the first token is a start token for a declaration, parse a declaration statement.
    if (auto token = lexer_.peek_token(); token && is_decl_start_token(*token)) {
        auto decl = safe_ptr<decl::decl_t>::make(parse_decl_internal());
        auto span = decl->base();
        return stmt::stmt_t{span, stmt::decl_t{std::move(decl)}};
    }

    // Otherwise we will have some kind of expression or assignment statement.
    // But assignment statements are expressions followed by an equal sign and another expression.
    auto expr = parse_expr_internal();

    if (expect_token<token_t::kind_e::p_equal>()) {
        auto              value_expr = parse_expr_internal();
        const node_base_t span{expr.base().begin(), value_expr.base().end()};
        return stmt::stmt_t{span,
                            stmt::assign_t{safe_ptr<expr::expr_t>::make(std::move(expr)),
                                           safe_ptr<expr::expr_t>::make(std::move(value_expr))}};
    }

    auto span = expr.base();
    return stmt::stmt_t{span, stmt::expr_t{safe_ptr<expr::expr_t>::make(std::move(expr))}};
}

} // namespace loxmocha::internal
