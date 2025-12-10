#pragma once

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"

#include <utility>
#include <vector>

namespace loxmocha::internal {

/**
 * @class parser_t
 *
 * @brief Internal parser implementation for parsing various AST nodes.
 *
 * This provides methods to parse declarations, expressions, patterns, statements and types.
 *
 * This is an internal class and should not be used outside of the parser library.
 */
class parser_t {
public:
    explicit parser_t(lexer_t& lexer) : lexer_(lexer) {}

    auto parse_decl() -> parser_result_t<decl::decl_t>;
    auto parse_expr() -> parser_result_t<expr::expr_t>;
    auto parse_pattern() -> parser_result_t<pattern::pattern_t>;
    auto parse_stmt() -> parser_result_t<stmt::stmt_t>;
    auto parse_type() -> parser_result_t<type::type_t>;

private:
    /**
     * @brief Check if the given token matches any of the specified kinds.
     *
     * @tparam Kinds The kinds to match against.
     * @param token The token to check.
     * @return true if the token matches any of the specified kinds, false otherwise.
     */
    template<token_t::kind_e... Kinds>
    auto match(const token_t& token) -> bool
    {
        static_assert(sizeof...(Kinds) > 0, "At least one kind must be provided");
        // cppcheck-suppress internalAstError
        return ((token.kind() == Kinds) || ...);
    }

    /**
     * @brief Expect the next token to be one of the specified kinds.
     *
     * If the next token matches any of the specified kinds, it is consumed and returned.
     * Otherwise, std::nullopt is returned.
     *
     * @tparam Kinds The kinds to expect.
     * @return std::optional<token_t> The expected token if it matches, std::nullopt otherwise.
     */
    template<token_t::kind_e... Kinds>
    auto expect_token() -> std::optional<token_t>
    {
        auto token = lexer_.peek_token();
        if (token && match<Kinds...>(*token)) {
            lexer_.consume_token();
            return *token;
        }

        return std::nullopt;
    }

    /**
     * @brief Parse a binary expression with the specified operator kinds.
     *
     * @tparam Kinds The operator kinds to parse.
     * @param parse_lhs A function that determines how to parse the lhs expression.
     * @param parse_rhs A function that determines how to parse the rhs expression.
     * @param construct_expr A function that determines how to construct the binary expression node from the operator
     * token, lhs expression and rhs expression.
     * @return expr::expr_t The parsed binary expression.
     */
    template<token_t::kind_e... Kinds>
    auto parse_binary_expr(auto&& parse_lhs, auto&& parse_rhs, auto&& construct_expr) -> expr::expr_t
    {
        auto left     = parse_lhs();
        auto op_token = lexer_.peek_token();
        while (op_token && match<Kinds...>(*op_token)) {
            lexer_.consume_token();
            auto right = parse_rhs();
            left       = construct_expr(*op_token, std::move(left), std::move(right));
            op_token   = lexer_.peek_token();
        }
        return left;
    }

    /**
     * @brief Parse a delimited list of elements.
     *
     * Parses elements until the end token is encountered, separated by the specified separator token.
     * This does not consume the end token.
     * If after parsing an element the separator is not found, parsing stops.
     *
     * @tparam end The kind of the end token.
     * @tparam separator The kind of the separator token.
     * @tparam T The type of the elements to parse.
     * @param parse_element A function that determines how to parse each element.
     * @return std::vector<T> The parsed elements.
     */
    template<token_t::kind_e end, token_t::kind_e separator, typename T>
    auto parse_delimited(auto&& parse_element) -> std::vector<T>
    {
        std::vector<T> elements{};

        for (;;) {
            auto element = lexer_.peek_token();
            if (element && match<end>(*element)) {
                break;
            }

            elements.emplace_back(parse_element());

            if (!expect_token<separator>()) {
                break;
            }
        }

        return elements;
    }

    /**
     * @brief Parse a delimited list of elements starting with a given start node.
     *
     * Parses elements until the end token is encountered, separated by the specified separator token.
     * This does not consume the end token.
     * If after parsing an element the separator is not found, parsing stops.
     *
     * @tparam end The kind of the end token.
     * @tparam separator The kind of the separator token.
     * @tparam T The type of the elements to parse.
     * @param start_node The first element to include in the result.
     * @param parse_element A function that determines how to parse each subsequent element.
     * @return std::vector<T> The parsed elements including the start node.
     */
    template<token_t::kind_e end, token_t::kind_e separator, typename T>
    auto parse_delimited(T&& start_node, auto&& parse_element) -> std::vector<T>
    {
        std::vector<T> elements{};
        elements.emplace_back(std::forward<T>(start_node));

        for (;;) {
            auto element = lexer_.peek_token();
            if (element && match<end>(*element)) {
                break;
            }

            elements.emplace_back(parse_element());

            if (!expect_token<separator>()) {
                break;
            }
        }

        return elements;
    }

    auto parse_decl_internal() -> decl::decl_t;
    auto parse_expr_internal() -> expr::expr_t;
    auto parse_pattern_internal() -> pattern::pattern_t;
    auto parse_stmt_internal() -> stmt::stmt_t;
    auto parse_type_internal() -> type::type_t;

    static auto is_decl_start_token(const token_t& token) -> bool;

    auto fun_decl() -> decl::decl_t;
    auto item_decl(decl::variable_t::mut_e mut) -> decl::decl_t;
    auto type_decl() -> decl::decl_t;

    auto if_expr() -> expr::expr_t;
    auto if_body() -> expr::expr_t;
    auto else_body() -> expr::expr_t;
    auto conditional_branch() -> expr::if_t::conditional_branch_t;

    auto while_expr() -> expr::expr_t;

    auto block_expr() -> expr::expr_t;
    auto block_body() -> expr::expr_t;

    auto or_expr() -> expr::expr_t;
    auto and_expr() -> expr::expr_t;
    auto equality_expr() -> expr::expr_t;
    auto comparison_expr() -> expr::expr_t;
    auto term_expr() -> expr::expr_t;
    auto factor_expr() -> expr::expr_t;
    auto is_expr() -> expr::expr_t;
    auto cast_expr() -> expr::expr_t;
    auto unary_expr() -> expr::expr_t;
    auto field_access_expr(expr::expr_t&& base_expr) -> expr::expr_t;
    auto index_expr(expr::expr_t&& base_expr) -> expr::expr_t;
    auto positional_args() -> std::vector<expr::expr_t>;
    auto named_args() -> std::vector<expr::call_t::named_arg_t>;
    auto call_expr(expr::expr_t&& callee_expr) -> expr::expr_t;
    auto access_expr() -> expr::expr_t;
    auto primary_expr() -> expr::expr_t;
    auto array_expr() -> expr::expr_t;
    auto record_expr() -> expr::expr_t;
    auto tuple_or_grouping_expr() -> expr::expr_t;

    auto tag_pattern() -> pattern::pattern_t;
    auto primary_pattern() -> pattern::pattern_t;

    auto expr_or_assign_stmt() -> stmt::stmt_t;
    auto decl_stmt() -> stmt::stmt_t;

    auto fun_type() -> type::type_t;
    auto ref_type() -> type::type_t;
    auto mutable_type() -> type::type_t;
    auto tagged_type() -> type::type_t;
    auto record_type() -> type::type_t;
    auto array_type() -> type::type_t;
    auto primary_type() -> type::type_t;
    auto tuple_or_grouping_type() -> type::type_t;

    // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
    lexer_t&                 lexer_;
    std::vector<std::string> diagnostics_;
    bool                     has_error_ = false;
};

} // namespace loxmocha::internal
