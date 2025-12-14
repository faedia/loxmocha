#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "parser-internal.hpp"

#include <cassert>
#include <string>
#include <utility>
#include <vector>

namespace loxmocha::internal {
namespace {
    auto construct_binary_expr(const token_t& op, expr::expr_t left, expr::expr_t right) -> expr::expr_t
    {
        return expr::expr_t{{left.base().begin(), right.base().end()},
                            expr::binary_t{op,
                                           safe_ptr<expr::expr_t>::make(std::move(left)),
                                           safe_ptr<expr::expr_t>::make(std::move(right))}};
    }
} // namespace

auto parser_t::parse_expr() -> parser_result_t<expr::expr_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<expr::expr_t>{parse_expr_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_expr_internal() -> expr::expr_t
{
    // Check to see if the start token of the expression is a control flow expression.
    // Otherwise parse it as a normal expression.
    switch (auto token = lexer_.peek_token(); token ? token->kind() : token_t::kind_e::s_eof) {
    case token_t::kind_e::k_if: lexer_.consume_token(); return if_expr(*token);
    case token_t::kind_e::k_while: lexer_.consume_token(); return while_expr(*token);
    case token_t::kind_e::k_begin: lexer_.consume_token(); return block_expr(*token);
    default: return or_expr();
    }
}

auto parser_t::if_expr(loxmocha::token_t if_tok) -> expr::expr_t
{
    // If expression parsing is complicated due to the multiple forms and mixing of block or expression bodies.
    // Along with optional dangling else branches, and the flat else-if structure.
    std::vector<expr::if_t::conditional_branch_t> conditional_branches{};
    // The entry point of this loop is just after consuming an 'if' token, which means we are about to expect a
    // condition. This makes the entry into this loop consistent.
    for (;;) {
        // Parse the condition.
        auto condition = or_expr();
        // If we have an arrow then we parse an expression.
        // Otherwise we expect a 'then' and parse a block.
        // Otherwise we have an invalid if expression and we report an error.
        if (expect_token<token_t::kind_e::p_arrow>()) {
            auto then_branch = parse_expr_internal();
            conditional_branches.emplace_back(expr::if_t::conditional_branch_t{.condition   = std::move(condition),
                                                                               .then_branch = std::move(then_branch)});

            // If we do not have an else after just parsing the branch, then the if expression is complete.
            if (!expect_token<token_t::kind_e::k_else>()) {
                const node_base_t span{if_tok.span().begin(), conditional_branches.back().then_branch.base().end()};
                return expr::expr_t{span, expr::if_t{std::move(conditional_branches)}};
            }
        } else if (auto then = expect_token<token_t::kind_e::k_then>(); then) {
            // Parse a block body.
            auto then_branch = block_body(*then);
            conditional_branches.emplace_back(expr::if_t::conditional_branch_t{.condition   = std::move(condition),
                                                                               .then_branch = std::move(then_branch)});

            // If we have an end token then the if expression is complete.
            if (auto end = expect_token<token_t::kind_e::k_end>(); end) {
                return expr::expr_t{{if_tok.span().begin(), end->span().end()},
                                    expr::if_t{std::move(conditional_branches)}};
            }

            // Otherwise we expect there to be an else token to continue the if expression.
            if (!expect_token<token_t::kind_e::k_else>()) {
                has_error_ = true;
                diagnostics_.emplace_back("Expected 'else' or 'end' after 'if' conditional branch");
                return expr::expr_t{"", expr::error_t{}};
            }

        } else {
            has_error_ = true;
            diagnostics_.emplace_back("Expected '=>' or 'then' after 'if' condition");
            return expr::expr_t{"", expr::error_t{}};
        }

        // If after parsing the branch and else token we do not have another if token then we only have the else
        // branch left.
        if (!expect_token<token_t::kind_e::k_if>()) {
            break;
        }
    }

    auto else_branch = else_body();

    const node_base_t span{if_tok.span().begin(), else_branch.base().end()};

    return expr::expr_t{
        span, expr::if_t{std::move(conditional_branches), safe_ptr<expr::expr_t>::make(std::move(else_branch))}};
}

auto parser_t::else_body() -> expr::expr_t
{
    // An else body can either be an expression or a block.
    if (expect_token<token_t::kind_e::p_arrow>()) {
        return parse_expr_internal();
    }
    auto block = lexer_.peek_token();
    if (block) {
        return block_expr(*block);
    }
    diagnostics_.emplace_back("Unknown token after 'else'");
    has_error_ = true;
    return expr::expr_t{"", expr::error_t{}};
}

auto parser_t::while_expr(loxmocha::token_t while_tok) -> expr::expr_t
{
    auto condition = or_expr();

    // if we have an arrow then we parse an expression body.
    if (expect_token<token_t::kind_e::p_arrow>()) {
        auto              body = parse_expr_internal();
        const node_base_t span{while_tok.span().begin(), body.base().end()};
        return expr::expr_t{span,
                            expr::while_t{safe_ptr<expr::expr_t>::make(std::move(condition)),
                                          safe_ptr<expr::expr_t>::make(std::move(body))}};
    }

    // Otherwise we expect a 'then' and parse a block body.
    auto then = expect_token<token_t::kind_e::k_then>();
    if (!then) {
        has_error_ = true;
        diagnostics_.emplace_back("Expected '=>' or 'then' after while condition");
        return expr::expr_t{"", expr::error_t{}};
    }

    auto              body = block_expr(*then);
    const node_base_t span{while_tok.span().begin(), body.base().end()};
    return expr::expr_t{span,
                        expr::while_t{safe_ptr<expr::expr_t>::make(std::move(condition)),
                                      safe_ptr<expr::expr_t>::make(std::move(body))}};
}

auto parser_t::block_expr(loxmocha::token_t begin) -> expr::expr_t
{
    // A block expression has already consumed its entry token.
    // This can be a begin token from an explicit block or a then token from an if or while expression.
    auto block = block_body(begin);

    // Block expression must terminate with an end token.
    auto end = expect_token<token_t::kind_e::k_end>();
    if (!end) {
        diagnostics_.emplace_back("Expected 'end' after block expression");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    block.base() = node_base_t{begin.span().begin(), end->span().end()};
    return block;
}

auto parser_t::block_body(loxmocha::token_t begin) -> expr::expr_t
{
    std::vector<stmt::stmt_t> statements{};
    // The default return expression is an empty tuple.
    auto return_expr =
        safe_ptr<expr::expr_t>::make(node_base_t{begin.span().end(), begin.span().end()}, expr::tuple_t{{}});

    for (;;) {
        // If we hit an end or else token then it means the block has terminated.
        // It may be odd that we check for else here, but this allows us to terminate
        // if-else blocks nicely.
        if (auto end_token = lexer_.peek_token();
            end_token && match<token_t::kind_e::k_end, token_t::kind_e::k_else>(*end_token)) {
            break;
        }

        auto stmt = parse_stmt_internal();

        if (auto semicolon = expect_token<token_t::kind_e::p_semicolon>(); semicolon) {
            statements.emplace_back(std::move(stmt));
            return_expr->base() = semicolon->span();
            continue;
        }

        // The final statement in a block must be an expression if it is not terminated by a semicolon.
        if (stmt.is<stmt::expr_t>()) {
            return_expr = std::move(stmt.as<stmt::expr_t>().expr());
            break;
        }

        has_error_ = true;
        diagnostics_.emplace_back("Expected ';' or expression at end of statement in block");
        return expr::expr_t{"", expr::error_t{}};
    }

    const node_base_t span{begin.span().begin(), return_expr->base().end()};
    return expr::expr_t{span, expr::block_t{std::move(statements), std::move(return_expr)}};
}

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
    // This breaks from our usual binary expression parsing
    // of lhs expr and rhs expr, since the rhs is a pattern.
    // and we are not constructing a binary expression node.
    return parse_binary_expr<token_t::kind_e::k_is>(
        [this]() -> expr::expr_t { return cast_expr(); },
        [this]() -> pattern::pattern_t { return parse_pattern_internal(); },
        [](token_t, expr::expr_t left, pattern::pattern_t right) -> expr::expr_t {
            const node_base_t span{left.base().begin(), right.base().end()};
            return expr::expr_t{span,
                                expr::is_t{safe_ptr<expr::expr_t>::make(std::move(left)),
                                           safe_ptr<pattern::pattern_t>::make(std::move(right))}};
        });
}

auto parser_t::cast_expr() -> expr::expr_t
{
    // Like is expressions, cast expressions break from our usual
    // binary expression parsing since the rhs is a type.
    return parse_binary_expr<token_t::kind_e::k_as>(
        [this]() -> expr::expr_t { return unary_expr(); },
        [this]() -> type::type_t { return parse_type_internal(); },
        [](token_t, expr::expr_t left, type::type_t right) -> expr::expr_t {
            const node_base_t span{left.base().begin(), right.base().end()};
            return expr::expr_t{span,
                                expr::cast_t{safe_ptr<expr::expr_t>::make(std::move(left)),
                                             safe_ptr<type::type_t>::make(std::move(right))}};
        });
}

auto parser_t::unary_expr() -> expr::expr_t
{
    if (auto op_token = expect_token<token_t::kind_e::p_bang, token_t::kind_e::p_minus>()) {
        auto              right = unary_expr();
        const node_base_t span{op_token->span().begin(), right.base().end()};
        return expr::expr_t{span, expr::unary_t{*op_token, safe_ptr<expr::expr_t>::make(std::move(right))}};
    }
    return access_expr();
}

auto parser_t::field_access_expr(expr::expr_t&& base_expr) -> expr::expr_t
{
    if (auto field = expect_token<token_t::kind_e::k_identifier>()) {
        const node_base_t span{base_expr.base().begin(), field->span().end()};
        return expr::expr_t{span, expr::field_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)), *field}};
    }

    diagnostics_.emplace_back("Expected identifier after '.'");
    has_error_ = true;
    return expr::expr_t{"", expr::error_t{}};
}

auto parser_t::index_expr(expr::expr_t&& base_expr) -> expr::expr_t
{
    auto index_expr = equality_expr();
    if (auto right_square = expect_token<token_t::kind_e::p_right_square>(); right_square) {
        const node_base_t span{base_expr.base().begin(), right_square->span().end()};
        return expr::expr_t{span,
                            expr::index_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)),
                                          safe_ptr<expr::expr_t>::make(std::move(index_expr))}};
    }

    diagnostics_.emplace_back("Expected ']' after index expression");
    has_error_ = true;
    return expr::expr_t{"", expr::error_t{}};
}

auto parser_t::positional_args() -> std::vector<expr::expr_t>
{
    std::vector<expr::expr_t> args{};
    for (;;) {
        // Check if we have the closing paren for the arg list, if so exit without consuming it.
        // call_expr is responsible for consuming the end paren.
        if (auto right_paren = lexer_.peek_token();
            right_paren && match<token_t::kind_e::p_right_paren>(*right_paren)) {
            break;
        }

        // If we have a identifier followed by a colon then we have a named argument.
        // We should stop parsing and backtrack to the name so that we can parse the named args.
        if (auto name = expect_token<token_t::kind_e::k_identifier>()) {
            if (auto colon = lexer_.peek_token(); colon && match<token_t::kind_e::p_colon>(*colon)) {
                lexer_.reset_token(*name);
                break;
            }
            lexer_.reset_token(*name);
        }

        // Otherwise have a positional arg!
        args.emplace_back(equality_expr());

        // If there is no comma then this is the last arg
        if (!expect_token<token_t::kind_e::p_comma>()) {
            break;
        }
    }
    return args;
}

auto parser_t::named_args() -> std::vector<expr::call_t::named_arg_t>
{
    std::vector<expr::call_t::named_arg_t> args{};

    for (;;) {
        // Check if we have the closing paren for the arg list, if so exit without consuming it.
        // call_expr is responsible for consuming the end paren.
        if (auto right_paren = lexer_.peek_token();
            right_paren && match<token_t::kind_e::p_right_paren>(*right_paren)) {
            break;
        }

        // We must have an identifier for the named argument.
        auto name = expect_token<token_t::kind_e::k_identifier>();
        if (!name) {
            diagnostics_.emplace_back("Expected identifier for named argument");
            has_error_ = true;
            return args;
        }

        // We must have a colon after the identifier.
        if (!expect_token<token_t::kind_e::p_colon>()) {
            diagnostics_.emplace_back("Expected ':' after named argument identifier");
            has_error_ = true;
            return args;
        }

        // Finally we can parse the expression for the named argument.
        args.emplace_back(*name, equality_expr());

        // If there is no comma then this is the last arg.
        if (!expect_token<token_t::kind_e::p_comma>()) {
            break;
        }
    }

    return args;
}

auto parser_t::call_expr(expr::expr_t&& callee_expr) -> expr::expr_t
{
    // Parse the arguments.
    auto positional = positional_args();
    auto named      = named_args();

    // Finally we must have a closing paren.
    auto right_paren = expect_token<token_t::kind_e::p_right_paren>();
    if (!right_paren) {
        diagnostics_.emplace_back("Expected ')' after arguments");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    const node_base_t span{callee_expr.base().begin(), right_paren->span().end()};
    return expr::expr_t{
        span,
        expr::call_t{safe_ptr<expr::expr_t>::make(std::move(callee_expr)), std::move(positional), std::move(named)}};
}

auto parser_t::access_expr() -> expr::expr_t
{
    auto base_expr = primary_expr();

    for (;;) {
        auto access_token =
            expect_token<token_t::kind_e::p_period, token_t::kind_e::p_left_square, token_t::kind_e::p_left_paren>();

        // If we do not have an access token then we are done.
        if (!access_token) {
            break;
        }

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
    // If we are at the end of input then we report an error.
    if (!lexer_.peek_token() || lexer_.peek_token()->kind() == token_t::kind_e::s_eof) {
        diagnostics_.emplace_back("Unexpected end of input");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    // Check for our highest precedence primary expression.
    // If we do not find one then we report an error.
    if (auto literal = expect_token<token_t::kind_e::l_integer,
                                    token_t::kind_e::l_string,
                                    token_t::kind_e::l_char,
                                    token_t::kind_e::k_true,
                                    token_t::kind_e::k_false>()) {
        return expr::expr_t{literal->span(), expr::literal_t{*literal}};
    }
    if (auto identifier = expect_token<token_t::kind_e::k_identifier>()) {
        return expr::expr_t{identifier->span(), expr::identifier_t(*identifier)};
    }
    if (auto left_square = expect_token<token_t::kind_e::p_left_square>(); left_square) {
        return array_expr(*left_square);
    }
    if (auto left_brace = expect_token<token_t::kind_e::p_left_brace>(); left_brace) {
        return record_expr(*left_brace);
    }
    if (auto left_paren = expect_token<token_t::kind_e::p_left_paren>(); left_paren) {
        return tuple_or_grouping_expr(*left_paren);
    }

    diagnostics_.emplace_back("Unexpected token: " + std::string(lexer_.peek_token()->span()));
    has_error_ = true;
    return expr::expr_t{"", expr::error_t{}};
}

auto parser_t::array_expr(const token_t& left_square) -> expr::expr_t
{
    auto elements = parse_delimited<token_t::kind_e::p_right_square, token_t::kind_e::p_comma, expr::expr_t>(
        [this]() -> expr::expr_t { return parse_expr_internal(); });

    // Expect the closing square bracket.
    auto right_square = expect_token<token_t::kind_e::p_right_square>();
    if (!right_square) {
        diagnostics_.emplace_back("Expected ']' after array elements");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    const node_base_t span{left_square.span().begin(), right_square->span().end()};
    return expr::expr_t{span, expr::array_t{std::move(elements)}};
}

auto parser_t::record_expr(const token_t& left_brace) -> expr::expr_t
{
    auto elements = parse_delimited<token_t::kind_e::p_right_brace, token_t::kind_e::p_comma, expr::record_t::field_t>(
        [this]() -> expr::record_t::field_t {
            // A record field consists of an identifier, a colon and an expression.
            auto name = expect_token<token_t::kind_e::k_identifier>();
            if (!name) {
                diagnostics_.emplace_back("Expected identifier for record field name");
                has_error_ = true;
                return expr::record_t::field_t{.name  = token_t::k_identifier("<error>"),
                                               .value = expr::expr_t{"", expr::error_t{}}};
            }

            if (!expect_token<token_t::kind_e::p_colon>()) {
                diagnostics_.emplace_back("Expected ':' after record field name");
                has_error_ = true;
                return expr::record_t::field_t{.name = *name, .value = expr::expr_t{"", expr::error_t{}}};
            }

            return expr::record_t::field_t{.name = *name, .value = parse_expr_internal()};
        });

    // Expect the closing brace.
    auto right_brace = expect_token<token_t::kind_e::p_right_brace>();
    if (!right_brace) {
        diagnostics_.emplace_back("Expected '}' after record fields");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    const node_base_t span{left_brace.span().begin(), right_brace->span().end()};
    return expr::expr_t{span, expr::record_t{std::move(elements)}};
}

auto parser_t::tuple_or_grouping_expr(const token_t& left_paren) -> expr::expr_t
{
    // Special case for empty tuple
    if (auto right_paren = expect_token<token_t::kind_e::p_right_paren>(); right_paren) {
        const node_base_t span{left_paren.span().begin(), right_paren->span().end()};
        return expr::expr_t{span, expr::tuple_t{{}}};
    }

    // The first expression, This is either the expression of a grouping or the first element of a tuple.
    auto first_expr = parse_expr_internal();

    // If we have a single element followed by a paren then its a grouping, otherwise we have some kind of tuple
    if (auto right_paren = expect_token<token_t::kind_e::p_right_paren>(); right_paren) {
        const node_base_t span{left_paren.span().begin(), right_paren->span().end()};
        return expr::expr_t{span, expr::grouping_t{safe_ptr<expr::expr_t>::make(std::move(first_expr))}};
    }

    // We must have a comma after the first expression to indicate a tuple.
    if (!expect_token<token_t::kind_e::p_comma>()) {
        diagnostics_.emplace_back("Expected ',' or ')' after expression");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    // Now parse the rest of tuple elements, constructing the vector with the first element.
    std::vector<expr::expr_t> elements =
        parse_delimited<token_t::kind_e::p_right_paren, token_t::kind_e::p_comma, expr::expr_t>(
            std::move(first_expr), [this]() -> expr::expr_t { return parse_expr_internal(); });

    // Expect the closing paren.
    auto right_paren = expect_token<token_t::kind_e::p_right_paren>();
    if (!right_paren) {
        diagnostics_.emplace_back("Expected ')' after expression");
        has_error_ = true;
        return expr::expr_t{"", expr::error_t{}};
    }

    const node_base_t span{left_paren.span().begin(), right_paren->span().end()};
    return expr::expr_t{span, expr::tuple_t{std::move(elements)}};
}

} // namespace loxmocha::internal
