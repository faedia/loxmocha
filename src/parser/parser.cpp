#include "loxmocha/ast/parser.hpp"

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/pattern.hpp"
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
            auto expr  = parse_expr_internal();
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

        auto parse_expr_internal() -> expr::expr_t { return or_expr(); }
        auto parse_pattern_internal() -> pattern::pattern_t { return tag_pattern(); }
        auto parse_type_internal() -> type::type_t
        {
            diagnostics_.emplace_back("Type parsing not implemented");
            has_error_ = true;
            return type::identifier_t{token_t::k_identifier("error")};
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

        static auto construct_binary_expr(token_t op, expr::expr_t left, expr::expr_t right) -> expr::expr_t
        {
            return expr::binary_t{
                op, safe_ptr<expr::expr_t>::make(std::move(left)), safe_ptr<expr::expr_t>::make(std::move(right))};
        }

        auto or_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::p_pipe_pipe>([this]() -> expr::expr_t { return and_expr(); },
                                                                   [this]() -> expr::expr_t { return and_expr(); },
                                                                   construct_binary_expr);
        }

        auto and_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::p_and_and>([this]() -> expr::expr_t { return equality_expr(); },
                                                                 [this]() -> expr::expr_t { return equality_expr(); },
                                                                 construct_binary_expr);
        }

        auto equality_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::p_equal_equal, token_t::kind_e::p_not_equal>(
                [this]() -> expr::expr_t { return comparison_expr(); },
                [this]() -> expr::expr_t { return comparison_expr(); },
                construct_binary_expr);
        }

        auto comparison_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::p_greater,
                                     token_t::kind_e::p_greater_equal,
                                     token_t::kind_e::p_less,
                                     token_t::kind_e::p_less_equal>([this]() -> expr::expr_t { return term_expr(); },
                                                                    [this]() -> expr::expr_t { return term_expr(); },
                                                                    construct_binary_expr);
        }

        auto term_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::p_plus, token_t::kind_e::p_minus>(
                [this]() -> expr::expr_t { return factor_expr(); },
                [this]() -> expr::expr_t { return factor_expr(); },
                construct_binary_expr);
        }

        auto factor_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::p_asterisk, token_t::kind_e::p_slash>(
                [this]() -> expr::expr_t { return is_expr(); },
                [this]() -> expr::expr_t { return is_expr(); },
                construct_binary_expr);
        }

        auto is_expr() -> expr::expr_t
        {
            return parse_binary_expr<token_t::kind_e::k_is>(
                [this]() -> expr::expr_t { return unary_expr(); },
                [this]() -> pattern::pattern_t { return parse_pattern_internal(); },
                [](token_t, expr::expr_t left, pattern::pattern_t right) -> expr::expr_t {
                    return expr::is_t{safe_ptr<expr::expr_t>::make(std::move(left)),
                                      safe_ptr<pattern::pattern_t>::make(std::move(right))};
                });
        }

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

        auto positional_args() -> std::vector<expr::expr_t>
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

        auto named_args() -> std::vector<expr::call_t::named_arg_t>
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

        auto call_expr(expr::expr_t&& callee_expr) -> expr::expr_t
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
            return expr::call_t{
                safe_ptr<expr::expr_t>::make(std::move(callee_expr)), std::move(positional), std::move(named)};
        }

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

        auto primary_expr() -> expr::expr_t
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

        auto tag_pattern() -> pattern::pattern_t
        {
            auto start_token = lexer_.peek_token();
            auto type        = parse_type_internal();

            auto dot_token  = lexer_.next_token();
            auto name_token = lexer_.next_token();

            if (dot_token && match<token_t::kind_e::p_period>(*dot_token) && name_token
                && match<token_t::kind_e::k_identifier>(*name_token)) {
                auto sub_pattern = primary_pattern();
                return pattern::tag_t{safe_ptr<type::type_t>::make(std::move(type)),
                                      *name_token,
                                      safe_ptr<pattern::pattern_t>::make(std::move(sub_pattern))};
            }

            lexer_.reset_token(*start_token);
            return primary_pattern();
        }

        auto primary_pattern() -> pattern::pattern_t
        {
            auto token = lexer_.peek_token();
            if (!token) {
                diagnostics_.emplace_back("Unexpected end of input");
                has_error_ = true;
                return pattern::error_t{};
            }

            if (match<token_t::kind_e::k_identifier>(*token)) {
                lexer_.consume_token();
                return pattern::identifier_t{*token};
            }

            diagnostics_.emplace_back("Unexpected token in pattern: " + std::string(token->span()));
            has_error_ = true;
            return pattern::error_t{};
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

auto parse_pattern([[maybe_unused]] lexer_t& lexer) -> parser_result_t<pattern::pattern_t>
{
    return parser_result_t<pattern::pattern_t>{pattern::identifier_t{token_t::k_identifier("my_var")}, false, {}};
}

} // namespace loxmocha
