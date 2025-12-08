#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <string>
#include <utility>

namespace loxmocha::internal {

auto parser_t::parse_pattern() -> parser_result_t<pattern::pattern_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<pattern::pattern_t>{parse_pattern_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_pattern_internal() -> pattern::pattern_t { return tag_pattern(); }

auto parser_t::tag_pattern() -> pattern::pattern_t
{
    auto start_token = lexer_.peek_token();
    auto type        = parse_type_internal();

    auto dot_token  = lexer_.next_token();
    auto name_token = lexer_.next_token();

    if (dot_token && match<token_t::kind_e::p_period>(*dot_token) && name_token
        && match<token_t::kind_e::k_identifier>(*name_token)) {
        auto sub_pattern = primary_pattern();
        return pattern::tag_t{safe_ptr<type::type_t>::make(std::move(type)),
                              *name_token,
                              safe_ptr<pattern::pattern_t>::make(std::move(sub_pattern))};
    }

    lexer_.reset_token(*start_token);
    return primary_pattern();
}

auto parser_t::primary_pattern() -> pattern::pattern_t
{
    auto token = lexer_.peek_token();
    if (!token) {
        diagnostics_.emplace_back("Unexpected end of input");
        has_error_ = true;
        return pattern::error_t{};
    }

    if (match<token_t::kind_e::k_identifier>(*token)) {
        lexer_.consume_token();
        return pattern::identifier_t{*token};
    }

    diagnostics_.emplace_back("Unexpected token in pattern: " + std::string(token->span()));
    has_error_ = true;
    return pattern::error_t{};
}

} // namespace loxmocha::internal
