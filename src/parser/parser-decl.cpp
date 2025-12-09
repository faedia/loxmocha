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
    case token_t::kind_e::k_fun: lexer_.consume_token(); return fun_decl();
    case token_t::kind_e::k_let: lexer_.consume_token(); return item_decl(decl::variable_t::mut_e::let);
    case token_t::kind_e::k_var: lexer_.consume_token(); return item_decl(decl::variable_t::mut_e::var);
    case token_t::kind_e::k_type: lexer_.consume_token(); return type_decl();
    default:
        has_error_ = true;
        diagnostics_.emplace_back("Expected declaration");
        return decl::error_t{"Invalid declaration"};
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

auto parser_t::fun_decl() -> decl::decl_t
{
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected function name in function declaration");
        return decl::error_t{"Invalid function declaration"};
    }

    if (!expect_token<token_t::kind_e::p_left_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '(' after function name in function declaration");
        return decl::error_t{"Invalid function declaration"};
    }

    auto params =
        parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, decl::function_t::parameter_t>(
            [this]() -> decl::function_t::parameter_t {
                auto param_name = expect_token<token_t::kind_e::k_identifier>();
                if (!param_name) {
                    has_error_ = true;
                    diagnostics_.emplace_back("Expected parameter name in function declaration");
                    return decl::function_t::parameter_t{
                        .name = token_t::k_identifier("ErrorParam"),
                        .type = safe_ptr<type::type_t>::make(type::identifier_t{token_t::k_identifier("ErrorType")})};
                }
                if (!expect_token<token_t::kind_e::p_colon>()) {
                    has_error_ = true;
                    diagnostics_.emplace_back("Expected ':' after parameter name in function declaration");
                    return decl::function_t::parameter_t{
                        .name = *param_name,
                        .type = safe_ptr<type::type_t>::make(type::identifier_t{token_t::k_identifier("ErrorType")})};
                }
                return decl::function_t::parameter_t{.name = *param_name,
                                                     .type = safe_ptr<type::type_t>::make(parse_type_internal())};
            });

    if (!expect_token<token_t::kind_e::p_right_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ')' after function parameters in function declaration");
        return decl::error_t{"Invalid function declaration"};
    }

    auto return_type = expect_token<token_t::kind_e::p_colon>() ? safe_ptr<type::type_t>::make(parse_type_internal())
                                                                : safe_ptr<type::type_t>::make(type::tuple_t{{}});

    if (expect_token<token_t::kind_e::p_arrow>()) {
        return decl::function_t{
            *name, std::move(params), std::move(return_type), safe_ptr<expr::expr_t>::make(parse_expr_internal())};
    }

    if (!expect_token<token_t::kind_e::k_begin>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'begin' or '=>' after function signature in function declaration");
        return decl::error_t{"Invalid function declaration"};
    }

    has_error_ = true;
    diagnostics_.emplace_back("Block bodies in function declarations are not yet supported");
    return decl::error_t{"Invalid function declaration"};
}

auto parser_t::item_decl(decl::variable_t::mut_e mut) -> decl::decl_t
{
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected variable name in variable declaration");
        return decl::error_t{"Invalid variable declaration"};
    }

    if (!expect_token<token_t::kind_e::p_colon>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ':' after variable name in variable declaration");
        return decl::error_t{"Invalid variable declaration"};
    }

    auto type = safe_ptr<type::type_t>::make(parse_type_internal());

    if (!expect_token<token_t::kind_e::p_equal>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '=' after variable type in variable declaration");
        return decl::error_t{"Invalid variable declaration"};
    }

    auto initialiser = safe_ptr<expr::expr_t>::make(parse_expr_internal());

    return decl::variable_t{mut, *name, std::move(type), std::move(initialiser)};
}

auto parser_t::type_decl() -> decl::decl_t
{
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected type name in type declaration");
        return decl::error_t{"Invalid type declaration"};
    }

    if (!expect_token<token_t::kind_e::k_is>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'is' after type name in type declaration");
        return decl::error_t{"Invalid type declaration"};
    }

    auto type = safe_ptr<type::type_t>::make(parse_type_internal());

    return decl::type_t{*name, std::move(type)};
}

} // namespace loxmocha::internal
