#include "loxmocha/ast/parser.hpp"

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <string>
#include <utility>
#include <vector>

namespace loxmocha {

namespace {

    class parser_t {
    public:
        explicit parser_t(lexer_t& lexer) : lexer_(lexer) {}

        auto parse_expr() -> parser_result_t<expr::expr_t>
        {
            diagnostics_.clear();
            has_error_ = false;
            auto expr  = equality_expr();
            return parser_result_t<expr::expr_t>{std::move(expr), has_error_, std::move(diagnostics_)};
        }

    private:
        template<token_t::kind_e kind, token_t::kind_e... Kinds>
        auto match(token_t token) -> bool
        {
            if (token.kind() == kind) {
                return true;
            }
            if constexpr (sizeof...(Kinds) > 0) {
                return match<Kinds...>(token);
            }
            return false;
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto equality_expr() -> expr::expr_t
        {
            auto left     = comparison_expr();
            auto op_token = lexer_.peek_token();
            while (op_token && match<token_t::kind_e::p_equal_equal, token_t::kind_e::p_not_equal>(*op_token)) {
                lexer_.consume_token();
                auto right = comparison_expr();
                left       = expr::binary_t{*op_token,
                                      safe_ptr<expr::expr_t>::make(std::move(left)),
                                      safe_ptr<expr::expr_t>::make(std::move(right))};
                op_token   = lexer_.peek_token();
            }
            return left;
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto comparison_expr() -> expr::expr_t
        {
            auto left     = term_expr();
            auto op_token = lexer_.peek_token();
            while (op_token
                   && match<token_t::kind_e::p_greater,
                            token_t::kind_e::p_greater_equal,
                            token_t::kind_e::p_less,
                            token_t::kind_e::p_less_equal>(*op_token)) {
                lexer_.consume_token();
                auto right = term_expr();
                left       = expr::binary_t{*op_token,
                                      safe_ptr<expr::expr_t>::make(std::move(left)),
                                      safe_ptr<expr::expr_t>::make(std::move(right))};
                op_token   = lexer_.peek_token();
            }

            return left;
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto term_expr() -> expr::expr_t
        {
            auto left     = factor_expr();
            auto op_token = lexer_.peek_token();
            while (op_token && match<token_t::kind_e::p_plus, token_t::kind_e::p_minus>(*op_token)) {
                lexer_.consume_token();
                auto right = factor_expr();
                left       = expr::binary_t{*op_token,
                                      safe_ptr<expr::expr_t>::make(std::move(left)),
                                      safe_ptr<expr::expr_t>::make(std::move(right))};
                op_token   = lexer_.peek_token();
            }
            return left;
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto factor_expr() -> expr::expr_t
        {
            auto left     = unary_expr();
            auto op_token = lexer_.peek_token();
            while (op_token && match<token_t::kind_e::p_asterisk, token_t::kind_e::p_slash>(*op_token)) {
                lexer_.consume_token();
                auto right = unary_expr();
                left       = expr::binary_t{*op_token,
                                      safe_ptr<expr::expr_t>::make(std::move(left)),
                                      safe_ptr<expr::expr_t>::make(std::move(right))};
                op_token   = lexer_.peek_token();
            }
            return left;
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto unary_expr() -> expr::expr_t
        {
            auto op_token = lexer_.peek_token();
            if (op_token && match<token_t::kind_e::p_bang, token_t::kind_e::p_minus>(*op_token)) {
                lexer_.consume_token();
                auto right = unary_expr();
                return expr::unary_t{*op_token, safe_ptr<expr::expr_t>::make(std::move(right))};
            }
            return access_expr();
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto access_expr() -> expr::expr_t
        {
            auto base_expr = primary_expr();

            auto next = lexer_.peek_token();

            while (next
                   && match<token_t::kind_e::p_period, token_t::kind_e::p_left_square, token_t::kind_e::p_left_paren>(
                       *next)) {
                lexer_.consume_token();

                if (match<token_t::kind_e::p_period>(*next)) {
                    next = lexer_.peek_token();
                    if (!next || !match<token_t::kind_e::k_identifier>(*next)) {
                        diagnostics_.emplace_back("Expected identifier after '.'");
                        has_error_ = true;
                        return expr::error_t{};
                    }
                    base_expr = expr::field_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)), *next};
                } else if (match<token_t::kind_e::p_left_square>(*next)) {
                    auto index_expr = equality_expr();
                    next            = lexer_.peek_token();
                    if (!next || !match<token_t::kind_e::p_right_square>(*next)) {
                        diagnostics_.emplace_back("Expected ']' after index expression");
                        has_error_ = true;
                        return expr::error_t{};
                    }
                    base_expr = expr::index_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)),
                                              safe_ptr<expr::expr_t>::make(std::move(index_expr))};

                } else if (match<token_t::kind_e::p_left_paren>(*next)) {
                    std::vector<expr::expr_t>                     positional_args{};
                    std::vector<std::pair<token_t, expr::expr_t>> named_args{};

                    next = lexer_.peek_token();
                    while (next && !match<token_t::kind_e::p_right_paren>(*next)) {
                        if (match<token_t::kind_e::k_identifier>(*next)) {
                            auto name = *next;
                            lexer_.consume_token();
                            next = lexer_.peek_token();
                            if (next && match<token_t::kind_e::p_colon>(*next)) {
                                lexer_.consume_token();
                                named_args.emplace_back(name, equality_expr());
                            } else {
                                lexer_.reset_token(name);
                                positional_args.emplace_back(equality_expr());
                            }
                        } else {
                            positional_args.emplace_back(equality_expr());
                        }

                        next = lexer_.peek_token();
                        if (next && match<token_t::kind_e::p_comma>(*next)) {
                            lexer_.consume_token();
                            next = lexer_.peek_token();
                        } else {
                            break;
                        }
                    }

                    if (!next || !match<token_t::kind_e::p_right_paren>(*next)) {
                        diagnostics_.emplace_back("Expected ')' after arguments");
                        has_error_ = true;
                        return expr::error_t{};
                    }
                    base_expr = expr::call_t{safe_ptr<expr::expr_t>::make(std::move(base_expr)),
                                             std::move(positional_args),
                                             std::move(named_args)};
                }
                lexer_.consume_token();
                next = lexer_.peek_token();
            }

            return base_expr;
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto primary_expr() -> expr::expr_t
        {
            if (!lexer_.peek_token()) {
                diagnostics_.emplace_back("Unexpected end of input");
                has_error_ = true;
                return expr::error_t{};
            }

            auto token = *lexer_.peek_token();
            if (match<token_t::kind_e::l_integer, token_t::kind_e::l_string, token_t::kind_e::l_char>(token)) {
                lexer_.consume_token();
                return expr::literal_t{token};
            }
            if (match<token_t::kind_e::k_identifier>(token)) {
                lexer_.consume_token();
                return expr::identifier_t(token);
            }
            if (match<token_t::kind_e::p_left_paren>(token)) {
                lexer_.consume_token();
                auto expr = equality_expr();
                auto next = lexer_.peek_token();
                if (next && !match<token_t::kind_e::p_right_paren>(*next)) {
                    diagnostics_.emplace_back("Expected ')' after expression");
                    has_error_ = true;
                    return expr::error_t{};
                }
                lexer_.consume_token();
                return expr::grouping_t{safe_ptr<expr::expr_t>::make(std::move(expr))};
            }

            diagnostics_.emplace_back("Unexpected token: " + std::string(token.span()));
            has_error_ = true;
            return expr::error_t{};
        }

        // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
        lexer_t&                 lexer_;
        std::vector<std::string> diagnostics_;
        bool                     has_error_ = false;
    };

} // namespace

auto parse_decl([[maybe_unused]] lexer_t& lexer) -> parser_result_t<decl::decl_t>
{
    return parser_result_t<decl::decl_t>{decl::error_t{"Not implemented"}, true, {"Not implemented"}};
}

auto parse_module([[maybe_unused]] lexer_t& lexer) -> parser_result_t<module::module_t>
{
    return parser_result_t<module::module_t>{module::module_t{}, true, {"Not implemented"}};
}

auto parse_stmt([[maybe_unused]] lexer_t& lexer) -> parser_result_t<stmt::stmt_t>
{
    stmt::stmt_t stmt = stmt::expr_t{safe_ptr<expr::expr_t>::make(expr::identifier_t(token_t::l_integer("42")))};
    return parser_result_t<stmt::stmt_t>{std::move(stmt), false, {}};
}

auto parse_expr([[maybe_unused]] lexer_t& lexer) -> parser_result_t<expr::expr_t>
{
    return parser_t{lexer}.parse_expr();
}

auto parse_type([[maybe_unused]] lexer_t& lexer) -> parser_result_t<type::type_t>
{
    return parser_result_t<type::type_t>{type::identifier_t(token_t::k_identifier("int")), false, {}};
}

} // namespace loxmocha
