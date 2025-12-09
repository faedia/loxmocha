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
    if (!expect_token<token_t::kind_e::k_choice>()) {
        return primary_pattern();
    }

    auto type = parse_type_internal();

    if (!expect_token<token_t::kind_e::p_period>()) {
        diagnostics_.emplace_back("Expected '.' after tag type in tag pattern");
        has_error_ = true;
        return pattern::error_t{};
    }

    auto name = expect_token<token_t::kind_e::k_identifier>();
    if (!name) {
        diagnostics_.emplace_back("Expected tag name in tag pattern");
        has_error_ = true;
        return pattern::error_t{};
    }

    auto tag = primary_pattern();

    return pattern::tag_t{
        safe_ptr<type::type_t>::make(std::move(type)), *name, safe_ptr<pattern::pattern_t>::make(std::move(tag))};
}

auto parser_t::primary_pattern() -> pattern::pattern_t
{
    if (auto token = expect_token<token_t::kind_e::k_identifier>(); token) {
        return pattern::identifier_t{*token};
    }
    if (expect_token<token_t::kind_e::p_left_paren>()) {
        auto pattern = parse_pattern_internal();
        if (!expect_token<token_t::kind_e::p_right_paren>()) {
            diagnostics_.emplace_back("Expected ')' after pattern");
            has_error_ = true;
            return pattern::error_t{};
        }
        return pattern;
    }

    diagnostics_.emplace_back("Expected pattern");
    has_error_ = true;
    return pattern::error_t{};
}

} // namespace loxmocha::internal
