#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <utility>
#include <vector>

namespace loxmocha::internal {

auto parser_t::parse_type() -> parser_result_t<type::type_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<type::type_t>{parse_type_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_type_internal() -> type::type_t
{
    // Check the first token to see if it is some entry for a complex type.
    // Otherwise parse some other type.
    switch (auto token = lexer_.peek_token(); token ? token->kind() : token_t::kind_e::s_eof) {
    case token_t::kind_e::k_fun: lexer_.consume_token(); return fun_type();
    case token_t::kind_e::k_let: lexer_.consume_token(); return ref_type();
    case token_t::kind_e::k_var: lexer_.consume_token(); return mutable_type();
    case token_t::kind_e::k_choice: lexer_.consume_token(); return tagged_type();
    case token_t::kind_e::k_rec: lexer_.consume_token(); return record_type();
    default: return array_type();
    }
}

auto parser_t::fun_type() -> type::type_t
{
    // Function types start with the fun token followed by a paren.
    if (!expect_token<token_t::kind_e::p_left_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '(' after 'fun' in function type");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }

    // Get the params after the function.
    auto param_types = parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, type::type_t>(
        [this]() -> type::type_t { return parse_type_internal(); });

    // If we ended without a closing paren then we have an error.
    if (!expect_token<token_t::kind_e::p_right_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ')' after function parameter types");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }

    // If we have a colon then we parse the return type, otherwise we default to the empty tuple.
    if (expect_token<token_t::kind_e::p_colon>()) {
        return type::function_t{std::move(param_types), safe_ptr<type::type_t>::make(parse_type_internal())};
    }
    return type::function_t{std::move(param_types), safe_ptr<type::type_t>::make(type::tuple_t{{}})};
}

auto parser_t::ref_type() -> type::type_t
{
    return type::reference_t{safe_ptr<type::type_t>::make(parse_type_internal())};
}

auto parser_t::mutable_type() -> type::type_t
{
    return type::mutable_t{safe_ptr<type::type_t>::make(parse_type_internal())};
}

auto parser_t::tagged_type() -> type::type_t
{
    // Tagged types must have at least one tag.
    if (expect_token<token_t::kind_e::k_end>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Tagged type must have at least one tag");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }

    auto tags = parse_delimited<token_t::kind_e::k_end, token_t::kind_e::p_comma, type::tagged_t::tag_t>(
        [this]() -> type::tagged_t::tag_t {
            // Each tag consists of an identifier, a colon and a type.
            auto name_token = expect_token<token_t::kind_e::k_identifier>();
            if (!name_token) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected tag name in tagged type");
                return type::tagged_t::tag_t{.name = token_t::k_identifier("ErrorTag"), .type = type::tuple_t{{}}};
            }

            if (!expect_token<token_t::kind_e::p_colon>()) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected ':' after tag name in tagged type");
                return type::tagged_t::tag_t{.name = *name_token, .type = type::tuple_t{{}}};
            }

            return type::tagged_t::tag_t{.name = *name_token, .type = parse_type_internal()};
        });

    // After parsing all the tags we must have an end token.
    if (!expect_token<token_t::kind_e::k_end>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'end' after tagged type definition");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }
    return type::tagged_t{std::move(tags)};
}

auto parser_t::record_type() -> type::type_t
{
    auto fields = parse_delimited<token_t::kind_e::k_end, token_t::kind_e::p_comma, type::record_t::field_t>(
        [this]() -> type::record_t::field_t {
            // A field consists of an identifier, a colon and a type.
            auto name_token = expect_token<token_t::kind_e::k_identifier>();
            if (!name_token) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected field name in record type");
                return type::record_t::field_t{.name = token_t::k_identifier("ErrorField"),
                                               .type = type::identifier_t{token_t::k_identifier("ErrorType")}};
            }
            if (!expect_token<token_t::kind_e::p_colon>()) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected ':' after field name in record type");
                return type::record_t::field_t{.name = *name_token,
                                               .type = type::identifier_t{token_t::k_identifier("ErrorType")}};
            }
            return type::record_t::field_t{.name = *name_token, .type = parse_type_internal()};
        });

    // If we ended without an end token then we have an error.
    if (!expect_token<token_t::kind_e::k_end>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'end' after record type definition");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }

    return type::record_t{std::move(fields)};
}

auto parser_t::array_type() -> type::type_t
{
    // Array types start with a primary type followed by some array size expressions.
    // Multiple array size expressions enable support for multi-dimensional arrays.
    auto base_type = primary_type();

    while (expect_token<token_t::kind_e::p_left_square>()) {
        auto size_expr = parse_expr_internal();
        if (!expect_token<token_t::kind_e::p_right_square>()) {
            has_error_ = true;
            diagnostics_.emplace_back("Expected ']' after array size expression");
            return type::identifier_t{token_t::k_identifier("ErrorType")};
        }
        base_type = type::array_t{safe_ptr<type::type_t>::make(std::move(base_type)),
                                  safe_ptr<expr::expr_t>::make(std::move(size_expr))};
    }

    return base_type;
}

auto parser_t::primary_type() -> type::type_t
{
    // If we have an identifier then we just have an identifier type.
    if (auto token = expect_token<token_t::kind_e::k_identifier>()) {
        return type::identifier_t{*token};
    }

    // Otherwise we expect a tuple or grouping.
    if (expect_token<token_t::kind_e::p_left_paren>()) {
        return tuple_or_grouping_type();
    }

    has_error_ = true;
    diagnostics_.emplace_back("Expected type expression");
    return type::identifier_t{token_t::k_identifier("ErrorType")};
}

auto parser_t::tuple_or_grouping_type() -> type::type_t
{
    // Special case for empty tuple
    if (expect_token<token_t::kind_e::p_right_paren>()) {
        return type::tuple_t{{}};
    }

    auto first_type = parse_type_internal();

    // If we have a single element followed by a paren then its a grouping, otherwise we have some kind of tuple
    if (expect_token<token_t::kind_e::p_right_paren>()) {
        return first_type;
    }

    // We must have a comma after the first type to indicate a tuple.
    if (!expect_token<token_t::kind_e::p_comma>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ',' or ')' in grouping type");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }

    // Now parse the rest of the types.
    std::vector<type::type_t> types =
        parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, type::type_t>(
            std::move(first_type), [this]() -> type::type_t { return parse_type_internal(); });

    // Expect the closing paren.
    if (!expect_token<token_t::kind_e::p_right_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ')' after grouping type");
        return type::identifier_t{token_t::k_identifier("ErrorType")};
    }

    return type::tuple_t{std::move(types)};
}

} // namespace loxmocha::internal
