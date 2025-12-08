#pragma once

#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"

#include <vector>

namespace loxmocha::internal {

class parser_t {
public:
    explicit parser_t(lexer_t& lexer) : lexer_(lexer) {}

    auto parse_expr() -> parser_result_t<expr::expr_t>;

private:
    template<token_t::kind_e... Kinds>
    auto match(const token_t& token) -> bool
    {
        static_assert(sizeof...(Kinds) > 0, "At least one kind must be provided");
        // cppcheck-suppress internalAstError
        return ((token.kind() == Kinds) || ...);
    }

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

            if (auto sep = lexer_.peek_token(); sep && match<separator>(*sep)) {
                lexer_.consume_token();
            } else {
                break;
            }
        }

        return elements;
    }

    auto parse_expr_internal() -> expr::expr_t;
    auto parse_pattern_internal() -> pattern::pattern_t;
    auto parse_type_internal() -> type::type_t;

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

    auto tag_pattern() -> pattern::pattern_t;
    auto primary_pattern() -> pattern::pattern_t;

    // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
    lexer_t&                 lexer_;
    std::vector<std::string> diagnostics_;
    bool                     has_error_ = false;
};

} // namespace loxmocha::internal
