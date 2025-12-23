#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <utility>
#include <vector>

namespace loxmocha::internal {

using namespace lexer;
using namespace ast;

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
    case token_t::kind_e::k_fun: lexer_.consume_token(); return fun_type(*token);
    case token_t::kind_e::k_let: lexer_.consume_token(); return ref_type(*token);
    case token_t::kind_e::k_var: lexer_.consume_token(); return mutable_type(*token);
    case token_t::kind_e::k_choice: lexer_.consume_token(); return tagged_type(*token);
    case token_t::kind_e::k_rec: lexer_.consume_token(); return record_type(*token);
    default: return array_type();
    }
}

auto parser_t::fun_type(const token_t& fun) -> type::type_t
{
    // Function types start with the fun token followed by a paren.
    if (!expect_token<token_t::kind_e::p_left_paren>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '(' after 'fun' in function type");
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    // Get the params after the function.
    auto param_types = parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, type::type_t>(
        [this]() -> type::type_t { return parse_type_internal(); });

    // If we ended without a closing paren then we have an error.
    auto right_paren = expect_token<token_t::kind_e::p_right_paren>();
    if (!right_paren) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ')' after function parameter types");
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    // If we have a colon then we parse the return type, otherwise we default to the empty tuple.
    if (expect_token<token_t::kind_e::p_colon>()) {
        auto              type = safe_ptr<type::type_t>::make(parse_type_internal());
        const node_base_t span{fun.span().begin(), type->base().end()};
        return type::type_t{span, type::function_t{std::move(param_types), std::move(type)}};
    }

    const node_base_t span{fun.span().begin(), right_paren->span().end()};
    auto return_type = safe_ptr<type::type_t>::make(node_base_t{right_paren->span().end(), right_paren->span().end()},
                                                    type::tuple_t{{}});

    return type::type_t{span, type::function_t{std::move(param_types), std::move(return_type)}};
}

auto parser_t::ref_type(const token_t& let) -> type::type_t
{
    auto              type = safe_ptr<type::type_t>::make(parse_type_internal());
    const node_base_t span{let.span().begin(), type->base().end()};
    return type::type_t{span, type::reference_t{std::move(type)}};
}

auto parser_t::mutable_type(const token_t& var) -> type::type_t
{
    auto              type = safe_ptr<type::type_t>::make(parse_type_internal());
    const node_base_t span{var.span().begin(), type->base().end()};
    return type::type_t{span, type::mutable_t{std::move(type)}};
}

auto parser_t::tagged_type(const token_t& choice) -> type::type_t
{
    // Tagged types must have at least one tag.
    if (expect_token<token_t::kind_e::k_end>()) {
        has_error_ = true;
        diagnostics_.emplace_back("Tagged type must have at least one tag");
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    auto tags = parse_delimited<token_t::kind_e::k_end, token_t::kind_e::p_comma, type::tagged_t::tag_t>(
        [this]() -> type::tagged_t::tag_t {
            // Each tag consists of an identifier, a colon and a type.
            auto name = expect_token<token_t::kind_e::k_identifier>();
            if (!name) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected tag name in tagged type");
                return type::tagged_t::tag_t{.name = token_t::k_identifier("<error>", identifier_t{0}),
                                             .type = type::type_t{"", type::tuple_t{{}}}};
            }

            if (!expect_token<token_t::kind_e::p_colon>()) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected ':' after tag name in tagged type");
                return type::tagged_t::tag_t{.name = *name, .type = type::type_t{"", type::tuple_t{{}}}};
            }

            return type::tagged_t::tag_t{.name = *name, .type = parse_type_internal()};
        });

    // After parsing all the tags we must have an end token.
    auto end = expect_token<token_t::kind_e::k_end>();
    if (!end) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'end' after tagged type definition");
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    const node_base_t span{choice.span().begin(), end->span().end()};
    return type::type_t{span, type::tagged_t{std::move(tags)}};
}

auto parser_t::record_type(const token_t& rec) -> type::type_t
{
    auto fields = parse_delimited<token_t::kind_e::k_end, token_t::kind_e::p_comma, type::record_t::field_t>(
        [this]() -> type::record_t::field_t {
            // A field consists of an identifier, a colon and a type.
            auto name_token = expect_token<token_t::kind_e::k_identifier>();
            if (!name_token) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected field name in record type");
                return type::record_t::field_t{
                    .name = token_t::k_identifier("ErrorField", identifier_t{0}),
                    .type = type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}}};
            }
            if (!expect_token<token_t::kind_e::p_colon>()) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected ':' after field name in record type");
                return type::record_t::field_t{
                    .name = *name_token,
                    .type = type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}}};
            }
            return type::record_t::field_t{.name = *name_token, .type = parse_type_internal()};
        });

    // If we ended without an end token then we have an error.
    auto end = expect_token<token_t::kind_e::k_end>();
    if (!end) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected 'end' after record type definition");
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    const node_base_t span{rec.span().begin(), end->span().end()};
    return type::type_t{span, type::record_t{std::move(fields)}};
}

auto parser_t::array_type() -> type::type_t
{
    // Array types start with a primary type followed by some array size expressions.
    // Multiple array size expressions enable support for multi-dimensional arrays.
    auto        base_type = primary_type();
    const auto* start     = base_type.base().begin();

    while (expect_token<token_t::kind_e::p_left_square>()) {
        auto size_expr    = parse_expr_internal();
        auto right_square = expect_token<token_t::kind_e::p_right_square>();
        if (!right_square) {
            has_error_ = true;
            diagnostics_.emplace_back("Expected ']' after array size expression");
            return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
        }
        const node_base_t span{start, right_square->span().end()};
        base_type = type::type_t{span,
                                 type::array_t{safe_ptr<type::type_t>::make(std::move(base_type)),
                                               safe_ptr<expr::expr_t>::make(std::move(size_expr))}};
    }

    return base_type;
}

auto parser_t::primary_type() -> type::type_t
{
    // If we have an identifier then we just have an identifier type.
    if (auto token = expect_token<token_t::kind_e::k_identifier>()) {
        return type::type_t{token->span(), type::identifier_t{*token}};
    }

    // Otherwise we expect a tuple or grouping.
    if (auto left_paren = expect_token<token_t::kind_e::p_left_paren>(); left_paren) {
        return tuple_or_grouping_type(*left_paren);
    }

    has_error_ = true;
    diagnostics_.emplace_back("Expected type expression");
    return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
}

auto parser_t::tuple_or_grouping_type(const token_t& left_paren) -> type::type_t
{
    // Special case for empty tuple
    if (auto right_paren = expect_token<token_t::kind_e::p_right_paren>(); right_paren) {
        return type::type_t{{left_paren.span().begin(), right_paren->span().end()}, type::tuple_t{{}}};
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
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    // Now parse the rest of the types.
    std::vector<type::type_t> types =
        parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, type::type_t>(
            std::move(first_type), [this]() -> type::type_t { return parse_type_internal(); });

    // Expect the closing paren.
    auto right_paren = expect_token<token_t::kind_e::p_right_paren>();
    if (!right_paren) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected ')' after grouping type");
        return type::type_t{"", type::identifier_t{token_t::k_identifier("ErrorType", identifier_t{0})}};
    }

    const node_base_t span{left_paren.span().begin(), right_paren->span().end()};
    return type::type_t{span, type::tuple_t{std::move(types)}};
}

} // namespace loxmocha::internal
