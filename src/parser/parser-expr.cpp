#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <algorithm>
#include <iterator>
#include <string>
#include <utility>
#include <vector>

namespace loxmocha::internal {
namespace {
    auto construct_binary_expr(const token_t& op, expr::expr_t left, expr::expr_t right) -> expr::expr_t
    {
        return expr::binary_t{
            op, safe_ptr<expr::expr_t>::make(std::move(left)), safe_ptr<expr::expr_t>::make(std::move(right))};
    }
} // namespace

auto parser_t::parse_expr() -> parser_result_t<expr::expr_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<expr::expr_t>{parse_expr_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_expr_internal() -> expr::expr_t { return or_expr(); }

auto parser_t::or_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::p_pipe_pipe>([this]() -> expr::expr_t { return and_expr(); },
                                                           [this]() -> expr::expr_t { return and_expr(); },
                                                           construct_binary_expr);
}

auto parser_t::and_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::p_and_and>([this]() -> expr::expr_t { return equality_expr(); },
                                                         [this]() -> expr::expr_t { return equality_expr(); },
                                                         construct_binary_expr);
}

auto parser_t::equality_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::p_equal_equal, token_t::kind_e::p_not_equal>(
        [this]() -> expr::expr_t { return comparison_expr(); },
        [this]() -> expr::expr_t { return comparison_expr(); },
        construct_binary_expr);
}

auto parser_t::comparison_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::p_greater,
                             token_t::kind_e::p_greater_equal,
                             token_t::kind_e::p_less,
                             token_t::kind_e::p_less_equal>([this]() -> expr::expr_t { return term_expr(); },
                                                            [this]() -> expr::expr_t { return term_expr(); },
                                                            construct_binary_expr);
}

auto parser_t::term_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::p_plus, token_t::kind_e::p_minus>(
        [this]() -> expr::expr_t { return factor_expr(); },
        [this]() -> expr::expr_t { return factor_expr(); },
        construct_binary_expr);
}

auto parser_t::factor_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::p_asterisk, token_t::kind_e::p_slash>(
        [this]() -> expr::expr_t { return is_expr(); },
        [this]() -> expr::expr_t { return is_expr(); },
        construct_binary_expr);
}

auto parser_t::is_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::k_is>(
        [this]() -> expr::expr_t { return cast_expr(); },
        [this]() -> pattern::pattern_t { return parse_pattern_internal(); },
        [](token_t, expr::expr_t left, pattern::pattern_t right) -> expr::expr_t {
            return expr::is_t{safe_ptr<expr::expr_t>::make(std::move(left)),
                              safe_ptr<pattern::pattern_t>::make(std::move(right))};
        });
}

auto parser_t::cast_expr() -> expr::expr_t
{
    return parse_binary_expr<token_t::kind_e::k_as>([this]() -> expr::expr_t { return unary_expr(); },
                                                    [this]() -> type::type_t { return parse_type_internal(); },
                                                    [](token_t, expr::expr_t left, type::type_t right) -> expr::expr_t {
                                                        return expr::cast_t{
                                                            safe_ptr<expr::expr_t>::make(std::move(left)),
                                                            safe_ptr<type::type_t>::make(std::move(right))};
                                                    });
}

auto parser_t::unary_expr() -> expr::expr_t
{
    auto op_token = lexer_.peek_token();
    if (op_token && match<token_t::kind_e::p_bang, token_t::kind_e::p_minus>(*op_token)) {
        lexer_.consume_token();
        auto right = unary_expr();
        return expr::unary_t{*op_token, safe_ptr<expr::expr_t>::make(std::move(right))};
    }
    return access_expr();
}

auto parser_t::field_access_expr(expr::expr_t&& base_expr) -> expr::expr_t
{
    auto field = lexer_.peek_token();
    if (!field || !match<token_t::kind_e::k_identifier>(*field)) {
        diagnostics_.emplace_back("Expected identifier after '.'");
        has_error_ = true;
        return expr::error_t{};
    }
    lexer_.consume_token();
    return expr::field_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)), *field};
}

auto parser_t::index_expr(expr::expr_t&& base_expr) -> expr::expr_t
{
    auto index_expr = equality_expr();
    auto end_square = lexer_.peek_token();
    if (!end_square || !match<token_t::kind_e::p_right_square>(*end_square)) {
        diagnostics_.emplace_back("Expected ']' after index expression");
        has_error_ = true;
        return expr::error_t{};
    }
    lexer_.consume_token();
    return expr::index_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)),
                         safe_ptr<expr::expr_t>::make(std::move(index_expr))};
}

auto parser_t::positional_args() -> std::vector<expr::expr_t>
{
    std::vector<expr::expr_t> args{};
    for (;;) {
        auto argument = lexer_.peek_token();

        if (!argument || match<token_t::kind_e::p_right_paren>(*argument)) {
            break;
        }
        if (match<token_t::kind_e::k_identifier>(*argument)) {
            auto name = *lexer_.next_token();
            if (auto colon = lexer_.peek_token(); colon && match<token_t::kind_e::p_colon>(*colon)) {
                lexer_.reset_token(name);
                break;
            }
            lexer_.reset_token(name);
        }
        args.emplace_back(equality_expr());

        auto comma = lexer_.peek_token();
        if (comma && match<token_t::kind_e::p_comma>(*comma)) {
            lexer_.consume_token();
        } else {
            break;
        }
    }
    return args;
}

auto parser_t::named_args() -> std::vector<expr::call_t::named_arg_t>
{
    std::vector<expr::call_t::named_arg_t> args{};

    for (;;) {
        auto name = lexer_.peek_token();
        if (!name || match<token_t::kind_e::p_right_paren>(*name)) {
            break;
        }
        if (!match<token_t::kind_e::k_identifier>(*name)) {
            diagnostics_.emplace_back("Expected identifier for named argument");
            has_error_ = true;
            return args;
        }

        lexer_.consume_token();

        auto colon = lexer_.peek_token();
        if (!colon || !match<token_t::kind_e::p_colon>(*colon)) {
            diagnostics_.emplace_back("Expected ':' after named argument identifier");
            has_error_ = true;
            return args;
        }

        lexer_.consume_token();

        args.emplace_back(*name, equality_expr());

        auto comma = lexer_.peek_token();
        if (comma && match<token_t::kind_e::p_comma>(*comma)) {
            lexer_.consume_token();
        } else {
            break;
        }
    }

    return args;
}

auto parser_t::call_expr(expr::expr_t&& callee_expr) -> expr::expr_t
{
    auto positional = positional_args();
    auto named      = named_args();

    auto end_paren = lexer_.peek_token();
    if (!end_paren || !match<token_t::kind_e::p_right_paren>(*end_paren)) {
        diagnostics_.emplace_back("Expected ')' after arguments");
        has_error_ = true;
        return expr::error_t{};
    }
    lexer_.consume_token();
    return expr::call_t{safe_ptr<expr::expr_t>::make(std::move(callee_expr)), std::move(positional), std::move(named)};
}

auto parser_t::access_expr() -> expr::expr_t
{
    auto base_expr = primary_expr();

    for (;;) {
        auto access_token = lexer_.peek_token();
        if (!access_token
            || !match<token_t::kind_e::p_period, token_t::kind_e::p_left_square, token_t::kind_e::p_left_paren>(
                *access_token)) {
            break;
        }
        lexer_.consume_token();

        if (match<token_t::kind_e::p_period>(*access_token)) {
            base_expr = field_access_expr(std::move(base_expr));
        } else if (match<token_t::kind_e::p_left_square>(*access_token)) {
            base_expr = index_expr(std::move(base_expr));
        } else if (match<token_t::kind_e::p_left_paren>(*access_token)) {
            base_expr = call_expr(std::move(base_expr));
        }
    }

    return base_expr;
}

auto parser_t::primary_expr() -> expr::expr_t
{
    if (!lexer_.peek_token() || lexer_.peek_token()->kind() == token_t::kind_e::s_eof) {
        diagnostics_.emplace_back("Unexpected end of input");
        has_error_ = true;
        return expr::error_t{};
    }

    auto token = *lexer_.peek_token();
    if (match<token_t::kind_e::l_integer,
              token_t::kind_e::l_string,
              token_t::kind_e::l_char,
              token_t::kind_e::k_true,
              token_t::kind_e::k_false>(token)) {
        lexer_.consume_token();
        return expr::literal_t{token};
    }
    if (match<token_t::kind_e::k_identifier>(token)) {
        lexer_.consume_token();
        return expr::identifier_t(token);
    }
    if (match<token_t::kind_e::p_left_square>(token)) {
        lexer_.consume_token();
        return array_expr();
    }
    if (match<token_t::kind_e::p_left_brace>(token)) {
        lexer_.consume_token();
        return record_expr();
    }
    if (match<token_t::kind_e::p_left_paren>(token)) {
        lexer_.consume_token();
        return tuple_or_grouping_expr();
    }

    diagnostics_.emplace_back("Unexpected token: " + std::string(token.span()));
    has_error_ = true;
    return expr::error_t{};
}

auto parser_t::array_expr() -> expr::expr_t
{
    auto elements = parse_delimited<token_t::kind_e::p_right_square, token_t::kind_e::p_comma, expr::expr_t>(
        [this]() -> expr::expr_t { return parse_expr_internal(); });

    if (auto end_square = lexer_.peek_token(); !end_square || !match<token_t::kind_e::p_right_square>(*end_square)) {
        diagnostics_.emplace_back("Expected ']' after array elements");
        has_error_ = true;
        return expr::error_t{};
    }

    lexer_.consume_token();

    return expr::array_t{std::move(elements)};
}

auto parser_t::record_expr() -> expr::expr_t
{
    auto elements = parse_delimited<token_t::kind_e::p_right_brace, token_t::kind_e::p_comma, expr::record_t::field_t>(
        [this]() -> expr::record_t::field_t {
            auto name = lexer_.peek_token();
            if (!name || !match<token_t::kind_e::k_identifier>(*name)) {
                diagnostics_.emplace_back("Expected identifier for record field name");
                has_error_ = true;
                return expr::record_t::field_t{.name = *name, .value = expr::error_t{}};
            }
            lexer_.consume_token();
            auto colon = lexer_.peek_token();
            if (!colon || !match<token_t::kind_e::p_colon>(*colon)) {
                diagnostics_.emplace_back("Expected ':' after record field name");
                has_error_ = true;
                return expr::record_t::field_t{.name = *name, .value = expr::error_t{}};
            }
            lexer_.consume_token();
            return expr::record_t::field_t{.name = *name, .value = parse_expr_internal()};
        });

    if (auto end_brace = lexer_.peek_token(); !end_brace || !match<token_t::kind_e::p_right_brace>(*end_brace)) {
        diagnostics_.emplace_back("Expected '}' after record fields");
        has_error_ = true;
        return expr::error_t{};
    }
    lexer_.consume_token();
    return expr::record_t{std::move(elements)};
}

auto parser_t::tuple_or_grouping_expr() -> expr::expr_t
{
    // Special case for empty tuple
    if (auto next = lexer_.peek_token(); next && match<token_t::kind_e::p_right_paren>(*next)) {
        lexer_.consume_token();
        return expr::tuple_t{{}};
    }

    auto first_expr = parse_expr_internal();

    // If we have a single element followed by a paren then its a grouping, otherwise we have some kind of tuple
    if (auto next = lexer_.peek_token(); next && match<token_t::kind_e::p_right_paren>(*next)) {
        lexer_.consume_token();
        return expr::grouping_t{safe_ptr<expr::expr_t>::make(std::move(first_expr))};
    }

    if (auto comma = lexer_.peek_token(); !comma || !match<token_t::kind_e::p_comma>(*comma)) {
        diagnostics_.emplace_back("Expected ',' or ')' after expression");
        has_error_ = true;
        return expr::error_t{};
    }
    lexer_.consume_token();

    std::vector<expr::expr_t> rest =
        parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, expr::expr_t>(
            [this]() -> expr::expr_t { return parse_expr_internal(); });

    if (auto end_paren = lexer_.peek_token(); !end_paren || !match<token_t::kind_e::p_right_paren>(*end_paren)) {
        diagnostics_.emplace_back("Expected ')' after expression");
        has_error_ = true;
        return expr::error_t{};
    }

    lexer_.consume_token();

    std::vector<expr::expr_t> elements{};
    elements.emplace_back(std::move(first_expr));
    std::ranges::transform(
        rest.begin(), rest.end(), std::back_inserter(elements), [](expr::expr_t& expr) { return std::move(expr); });

    return expr::tuple_t{std::move(elements)};
}

} // namespace loxmocha::internal
