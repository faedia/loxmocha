#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"
#include <cstddef>
#include <print>
#include <string>
#include <utility>
#include <vector>

TEST(ExprTest, Literal)
{
    loxmocha::expr::literal_t literal{loxmocha::token_t::l_integer("42")};

    loxmocha::expr::expr_t expr = literal;

    ASSERT_TRUE(expr.is<loxmocha::expr::literal_t>());
    auto& lit = expr.as<loxmocha::expr::literal_t>();
    ASSERT_EQ(lit.value().kind(), loxmocha::token_t::kind_e::l_integer);
}

class SimplePrettyPrinter {
public:
    [[nodiscard]] static auto create_indent(const std::vector<bool>& lastChildPath) -> std::string
    {
        if (lastChildPath.empty()) {
            return "";
        }

        std::string indent;
        for (size_t i = 0; i < lastChildPath.size() - 1; ++i) {
            indent += lastChildPath[i] ? "  " : "│ ";
        }
        indent += lastChildPath.back() ? "└─" : "├─";
        return indent;
    }

    void operator()([[maybe_unused]] const auto& e, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Unknown Expr\n", create_indent(lastChildPath));
    }

    void operator()(const loxmocha::expr::field_t& field, std::vector<bool> lastChildPath = {})
    {
        std::print("{}┬Field: {}\n", create_indent(lastChildPath), field.field_name().span());
        lastChildPath.push_back(true);
        field.base()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const loxmocha::expr::index_t& index, std::vector<bool> lastChildPath = {})
    {
        std::print("{}┬Index:\n", create_indent(lastChildPath));
        lastChildPath.push_back(false);
        index.base()->visit(*this, lastChildPath);
        lastChildPath.back() = true;
        index.index()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const loxmocha::expr::call_t& call, std::vector<bool> lastChildPath = {})
    {
        std::print("{}┬Call:\n", create_indent(lastChildPath));
        lastChildPath.push_back(call.positional_args().empty() && call.named_args().empty());
        call.callee()->visit(*this, lastChildPath);

        for (size_t i = 0; i < call.positional_args().size(); ++i) {
            lastChildPath.back() = (i == call.positional_args().size() - 1) && call.named_args().empty();
            call.positional_args()[i].visit(*this, lastChildPath);
        }

        for (size_t i = 0; i < call.named_args().size(); ++i) {
            lastChildPath.back() = (i == call.named_args().size() - 1);
            std::print("{}┬Named Arg: {}\n", create_indent(lastChildPath), call.named_args()[i].name.span());
            lastChildPath.push_back(true);
            call.named_args()[i].value.visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const loxmocha::expr::literal_t& lit, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Literal: {}\n", create_indent(lastChildPath), lit.value().span());
    }

    void operator()(const loxmocha::expr::identifier_t& id, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Identifier: {}\n", create_indent(lastChildPath), id.name().span());
    }

    void operator()(const loxmocha::expr::binary_t& bin, std::vector<bool> lastChildPath = {})
    {
        std::print("{}┬Binary: {}\n", create_indent(lastChildPath), bin.op().kind());
        lastChildPath.push_back(false);
        bin.left()->visit(*this, lastChildPath);
        lastChildPath.back() = true;
        bin.right()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

private:
};

TEST(ExprTest, BinaryExpr)
{
    loxmocha::expr::literal_t left_literal{loxmocha::token_t::l_integer("42")};
    loxmocha::expr::literal_t right_literal{loxmocha::token_t::l_integer("58")};

    loxmocha::expr::binary_t binary_expr{loxmocha::token_t::p_plus("+"),
                                         loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(left_literal),
                                         loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(right_literal)};

    loxmocha::expr::expr_t expr = std::move(binary_expr);

    ASSERT_TRUE(expr.is<loxmocha::expr::binary_t>());
    auto& bin_expr = expr.as<loxmocha::expr::binary_t>();
    ASSERT_EQ(bin_expr.op().kind(), loxmocha::token_t::kind_e::p_plus);

    expr.visit(SimplePrettyPrinter{});

    expr.as<loxmocha::expr::binary_t>().left() = loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(
        loxmocha::expr::binary_t(loxmocha::token_t::p_minus("-"),
                                 loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(
                                     loxmocha::expr::literal_t{loxmocha::token_t::l_integer("100")}),
                                 loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(
                                     loxmocha::expr::identifier_t{loxmocha::token_t::k_identifier("num")})));
    expr.visit(SimplePrettyPrinter{});
}

TEST(ExprTest, ParserTest)
{
    const auto*       parse_string = "42 + 58 * ident - num";
    loxmocha::lexer_t lexer{parse_string};
    auto              result = loxmocha::parse_expr(lexer);

    std::println("parsing '{}'", parse_string);
    result.result().visit(SimplePrettyPrinter{});
    for (const auto& diag : result.diagnostics()) {
        std::print("Diagnostic: {}", diag);
    }
}

TEST(ExprTest, ParserFieldTest)
{
    const auto*       parse_string = "a.b.c + 58 * ident - num";
    loxmocha::lexer_t lexer{parse_string};
    auto              result = loxmocha::parse_expr(lexer);

    std::println("parsing '{}'", parse_string);
    result.result().visit(SimplePrettyPrinter{});
    for (const auto& diag : result.diagnostics()) {
        std::print("Diagnostic: {}", diag);
    }
}

TEST(ExprTest, ParserCallTest)
{
    const auto*       parse_string = "func(a, b + c, d * 42, named: a.b.c, other: arr[a+2].f().a)";
    loxmocha::lexer_t lexer{parse_string};
    auto              result = loxmocha::parse_expr(lexer);

    std::println("parsing '{}'", parse_string);
    result.result().visit(SimplePrettyPrinter{});
    for (const auto& diag : result.diagnostics()) {
        std::print("Diagnostic: {}", diag);
    }
}
