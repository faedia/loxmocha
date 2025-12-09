#pragma once

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"

#include <vector>

namespace loxmocha::internal {

class parser_t {
public:
    explicit parser_t(lexer_t& lexer) : lexer_(lexer) {}

    auto parse_decl() -> parser_result_t<decl::decl_t>;
    auto parse_expr() -> parser_result_t<expr::expr_t>;
    auto parse_pattern() -> parser_result_t<pattern::pattern_t>;
    auto parse_stmt() -> parser_result_t<stmt::stmt_t>;
    auto parse_type() -> parser_result_t<type::type_t>;

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
