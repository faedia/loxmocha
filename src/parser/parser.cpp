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
#include <variant>
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
        template<token_t::kind_e... Kinds>
        auto match(token_t token) -> bool
        {
            static_assert(sizeof...(Kinds) > 0, "At least one kind must be provided");
            // cppcheck-suppress internalAstError
            return ((token.kind() == Kinds) || ...);
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

        auto field_access_expr(expr::expr_t&& base_expr) -> expr::expr_t
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

        auto index_expr(expr::expr_t&& base_expr) -> expr::expr_t
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

        auto named_arg() -> std::variant<expr::expr_t, std::pair<token_t, expr::expr_t>>
        {
            auto name = *lexer_.next_token();
            if (auto colon = lexer_.peek_token(); colon && match<token_t::kind_e::p_colon>(*colon)) {
                lexer_.consume_token();
                return std::make_pair(name, equality_expr());
            }
            lexer_.reset_token(name);
            return equality_expr();
        }

        auto call_expr(expr::expr_t&& callee_expr) -> expr::expr_t
        {
            std::vector<expr::expr_t>                     positional_args{};
            std::vector<std::pair<token_t, expr::expr_t>> named_args{};

            for (;;) {
                auto argument = lexer_.peek_token();
                if (!argument || match<token_t::kind_e::p_right_paren>(*argument)) {
                    break;
                }

                if (match<token_t::kind_e::k_identifier>(*argument)) {
                    auto result = named_arg();
                    if (std::holds_alternative<expr::expr_t>(result)) {
                        positional_args.emplace_back(std::move(std::get<expr::expr_t>(result)));
                    } else {
                        named_args.emplace_back(std::move(std::get<std::pair<token_t, expr::expr_t>>(result)));
                    }
                } else {
                    positional_args.emplace_back(equality_expr());
                }

                auto comma = lexer_.peek_token();
                if (comma && match<token_t::kind_e::p_comma>(*comma)) {
                    lexer_.consume_token();
                } else {
                    break;
                }
            }

            auto end_paren = lexer_.peek_token();
            if (!end_paren || !match<token_t::kind_e::p_right_paren>(*end_paren)) {
                diagnostics_.emplace_back("Expected ')' after arguments");
                has_error_ = true;
                return expr::error_t{};
            }
            lexer_.consume_token();
            return expr::call_t{safe_ptr<expr::expr_t>::make(std::move(callee_expr)),
                                std::move(positional_args),
                                std::move(named_args)};
        }

        // NOLINTNEXTLINE(misc-no-recursion)
        auto access_expr() -> expr::expr_t
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

auto parse_expr(lexer_t& lexer) -> parser_result_t<expr::expr_t> { return parser_t{lexer}.parse_expr(); }

auto parse_type([[maybe_unused]] lexer_t& lexer) -> parser_result_t<type::type_t>
{
    return parser_result_t<type::type_t>{type::identifier_t(token_t::k_identifier("int")), false, {}};
}

} // namespace loxmocha
