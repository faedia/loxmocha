#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
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
    switch (auto token = lexer_.peek_token(); token ? token->kind() : token_t::kind_e::s_eof) {
    case token_t::kind_e::k_fun: lexer_.consume_token(); return fun_decl(*token);
    case token_t::kind_e::k_let: lexer_.consume_token(); return item_decl(*token, decl::variable_t::mut_e::let);
    case token_t::kind_e::k_var: lexer_.consume_token(); return item_decl(*token, decl::variable_t::mut_e::var);
    case token_t::kind_e::k_type: lexer_.consume_token(); return type_decl(*token);
    default:
        lexer_.consume_token();
        has_error_ = true;
        diagnostics_.emplace_back("Expected declaration");
        return decl::decl_t{node_base_t{token->span()}, decl::error_t{"Invalid declaration"}};
    }
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

auto parser_t::fun_decl(loxmocha::token_t fun) -> decl::decl_t
{
    // We expect a function to start with an identifier and an opening paren.
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected function name in function declaration");
        return decl::decl_t{"", decl::error_t{"Invalid function declaration"}};
    }
    if (!expect_token<token_t::kind_e::p_left_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '(' after function name in function declaration");
        return decl::decl_t{"", decl::error_t{"Invalid function declaration"}};
    }

    auto params =
        parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, decl::function_t::parameter_t>(
            [this]() -> decl::function_t::parameter_t {
                // A parameter consists of an identifier, a colon and a type.
                // If we don't have a name or a colon then we report an error.
                auto param_name = expect_token<token_t::kind_e::k_identifier>();
                if (!param_name) {
                    has_error_ = true;
                    diagnostics_.emplace_back("Expected parameter name in function declaration");
                    return decl::function_t::parameter_t{
                        .name = token_t::k_identifier("<error>"),
                        .type = type::type_t{"", type::identifier_t{token_t::k_identifier("<error>")}}};
                }
                if (!expect_token<token_t::kind_e::p_colon>()) {
                    has_error_ = true;
                    diagnostics_.emplace_back("Expected ':' after parameter name in function declaration");
                    return decl::function_t::parameter_t{
                        .name = *param_name,
                        .type = type::type_t{"", type::identifier_t{token_t::k_identifier("<error>")}}};
                }
                return decl::function_t::parameter_t{.name = *param_name, .type = parse_type_internal()};
            });

    auto right_paren_token = expect_token<token_t::kind_e::p_right_paren>();

    // Now we expect and consume the closing parent.
    if (!right_paren_token) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ')' after function parameters in function declaration");
        return decl::decl_t{"", decl::error_t{"Invalid function declaration"}};
    }

    // If we have a colon then we parse the return type, otherwise we default to the empty tuple.
    auto return_type =
        expect_token<token_t::kind_e::p_colon>()
            ? safe_ptr<type::type_t>::make(parse_type_internal())
            : safe_ptr<type::type_t>::make(
                  node_base_t{right_paren_token->span().end(), right_paren_token->span().end()}, type::tuple_t{{}});

    // We have two kinds of bodies for function.
    // We either have an expression body which has the entry token '=>'
    if (expect_token<token_t::kind_e::p_arrow>()) {
        auto body = parse_expr_internal();
        return decl::decl_t{
            node_base_t{fun.span().begin(), body.base().end()},
            decl::function_t{
                *name, std::move(params), std::move(return_type), safe_ptr<expr::expr_t>::make(std::move(body))}};
    }

    // Otherwise we have a block body which has the entry token 'begin'.
    // If we do not have a 'begin' token then we will report an error.
    auto begin = expect_token<token_t::kind_e::k_begin>();
    if (!begin) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'begin' or '=>' after function signature in function declaration");
        return decl::decl_t{"", decl::error_t{"Invalid function declaration"}};
    }

    auto body = block_expr(*begin);

    return decl::decl_t{
        node_base_t{fun.span().begin(), body.base().end()},
        decl::function_t{
            *name, std::move(params), std::move(return_type), safe_ptr<expr::expr_t>::make(std::move(body))}};
}

auto parser_t::item_decl(const token_t& let, decl::variable_t::mut_e mut) -> decl::decl_t
{
    // We expect a variable to start with an identifier, a colon, a type, an equal sign and an initialiser.
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected variable name in variable declaration");
        return decl::decl_t{"", decl::error_t{"Invalid variable declaration"}};
    }

    if (!expect_token<token_t::kind_e::p_colon>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ':' after variable name in variable declaration");
        return decl::decl_t{"", decl::error_t{"Invalid variable declaration"}};
    }

    auto type = safe_ptr<type::type_t>::make(parse_type_internal());

    if (!expect_token<token_t::kind_e::p_equal>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '=' after variable type in variable declaration");
        return decl::decl_t{"", decl::error_t{"Invalid variable declaration"}};
    }

    auto initialiser = safe_ptr<expr::expr_t>::make(parse_expr_internal());

    return decl::decl_t{{let.span().begin(), initialiser->base().end()},
                        decl::variable_t{mut, *name, std::move(type), std::move(initialiser)}};
}

auto parser_t::type_decl(loxmocha::token_t type) -> decl::decl_t
{
    // We expect a type to start with an identifier, an 'is' keyword and a type.
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected type name in type declaration");
        return decl::decl_t{"", decl::error_t{"Invalid type declaration"}};
    }

    if (!expect_token<token_t::kind_e::k_is>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'is' after type name in type declaration");
        return decl::decl_t{"", decl::error_t{"Invalid type declaration"}};
    }

    auto type_expr = safe_ptr<type::type_t>::make(parse_type_internal());

    return decl::decl_t{{type.span().begin(), type_expr->base().end()}, decl::type_t{*name, std::move(type_expr)}};
}

} // namespace loxmocha::internal
