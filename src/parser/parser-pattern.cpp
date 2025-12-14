#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

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
    // Tag patterns start with the 'choice' keyword.
    // If not present, we parse a primary pattern.
    if (!expect_token<token_t::kind_e::k_choice>()) {
        return primary_pattern();
    }

    // Get the type of the tag.
    auto type = parse_type_internal();

    // We must have a period after the type.
    if (!expect_token<token_t::kind_e::p_period>()) {
        diagnostics_.emplace_back("Expected '.' after tag type in tag pattern");
        has_error_ = true;
        return pattern::pattern_t{"", pattern::error_t{}};
    }

    // Get the tag name.
    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        diagnostics_.emplace_back("Expected tag name in tag pattern");
        has_error_ = true;
        return pattern::pattern_t{"", pattern::error_t{}};
    }

    // Process the sub-pattern.
    auto pattern = primary_pattern();

    const node_base_t span{type.base().begin(), pattern.base().end()};
    return pattern::pattern_t{span,
                              pattern::tag_t{safe_ptr<type::type_t>::make(std::move(type)),
                                             *name,
                                             safe_ptr<pattern::pattern_t>::make(std::move(pattern))}};
}

auto parser_t::primary_pattern() -> pattern::pattern_t
{
    // An identifier pattern is just an identifier.
    if (auto token = expect_token<token_t::kind_e::k_identifier>(); token) {
        return pattern::pattern_t{token->span(), pattern::identifier_t{*token}};
    }
    // Otherwise we have a paren to denote a nested pattern.
    if (expect_token<token_t::kind_e::p_left_paren>()) {
        auto pattern = parse_pattern_internal();
        if (!expect_token<token_t::kind_e::p_right_paren>()) {
            diagnostics_.emplace_back("Expected ')' after pattern");
            has_error_ = true;
            return pattern::pattern_t{"", pattern::error_t{}};
        }
        return pattern;
    }

    diagnostics_.emplace_back("Expected pattern");
    has_error_ = true;
    return pattern::pattern_t{"", pattern::error_t{}};
}

} // namespace loxmocha::internal
