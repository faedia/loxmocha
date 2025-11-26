#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"
#include <cstddef>
#include <print>
#include <string>
#include <type_traits>
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

class expr_assert_visitor {
public:
    void operator()([[maybe_unused]] const auto& actual, [[maybe_unused]] const auto& expected)
    {
        FAIL() << "Unexpected expression type encountered in assertion visitor.";
    }

    void operator()(const auto& actual, const loxmocha::expr::expr_t& expected)
    {
        ASSERT_TRUE(expected.is<std::decay_t<decltype(actual)>>());
        this->operator()(actual, expected.as<std::decay_t<decltype(actual)>>());
    }

    void operator()(const loxmocha::expr::literal_t& actual, const loxmocha::expr::literal_t& expected)
    {
        ASSERT_TRUE(actual.value().kind() == loxmocha::token_t::kind_e::l_integer
                    || actual.value().kind() == loxmocha::token_t::kind_e::l_char
                    || actual.value().kind() == loxmocha::token_t::kind_e::l_string);

        ASSERT_EQ(actual.value().kind(), expected.value().kind());
        ASSERT_EQ(actual.value().span(), expected.value().span());
    }

    void operator()(const loxmocha::expr::identifier_t& actual, const loxmocha::expr::identifier_t& expected)
    {
        ASSERT_EQ(actual.name().kind(), expected.name().kind());
        ASSERT_EQ(actual.name().kind(), expected.name().kind());
        ASSERT_EQ(actual.name().span(), expected.name().span());
    }

    void operator()(const loxmocha::expr::binary_t& actual, const loxmocha::expr::binary_t& expected)
    {
        ASSERT_EQ(actual.op().kind(), expected.op().kind());
        actual.left()->visit(*this, *expected.left());
        actual.right()->visit(*this, *expected.right());
    }

    void operator()(const loxmocha::expr::unary_t& actual, const loxmocha::expr::unary_t& expected)
    {
        ASSERT_EQ(actual.op().kind(), expected.op().kind());
        actual.operand()->visit(*this, *expected.operand());
    }

    void operator()(const loxmocha::expr::array_t& actual, const loxmocha::expr::array_t& expected)
    {
        ASSERT_EQ(actual.elements().size(), expected.elements().size());
        for (size_t i = 0; i < actual.elements().size(); ++i) {
            actual.elements()[i].visit(*this, expected.elements()[i]);
        }
    }

    void operator()(const loxmocha::expr::is_t& actual, const loxmocha::expr::is_t& expected)
    {
        actual.expr()->visit(*this, *expected.expr());
        actual.type()->visit(*this, *expected.type());
    }

    void operator()(const loxmocha::expr::cast_t& actual, const loxmocha::expr::cast_t& expected)
    {
        actual.expr()->visit(*this, *expected.expr());
        actual.type()->visit(*this, *expected.type());
    }

    void operator()(const loxmocha::expr::tuple_t& actual, const loxmocha::expr::tuple_t& expected)
    {
        ASSERT_EQ(actual.elements().size(), expected.elements().size());
        for (size_t i = 0; i < actual.elements().size(); ++i) {
            actual.elements()[i].visit(*this, expected.elements()[i]);
        }
    }

    void operator()(const loxmocha::expr::record_t& actual, const loxmocha::expr::record_t& expected)
    {
        ASSERT_EQ(actual.fields().size(), expected.fields().size());
        for (size_t i = 0; i < actual.fields().size(); ++i) {
            ASSERT_EQ(actual.fields()[i].name.kind(), loxmocha::token_t::kind_e::k_identifier);
            ASSERT_EQ(actual.fields()[i].name.kind(), expected.fields()[i].name.kind());
            ASSERT_EQ(actual.fields()[i].name.span(), expected.fields()[i].name.span());
            actual.fields()[i].value.visit(*this, expected.fields()[i].value);
        }
    }

    void operator()(const loxmocha::expr::index_t& actual, const loxmocha::expr::index_t& expected)
    {
        actual.base()->visit(*this, *expected.base());
        actual.index()->visit(*this, *expected.index());
    }

    void operator()(const loxmocha::expr::field_t& actual, const loxmocha::expr::field_t& expected)
    {
        ASSERT_EQ(actual.field_name().kind(), loxmocha::token_t::kind_e::k_identifier);
        ASSERT_EQ(actual.field_name().kind(), expected.field_name().kind());
        ASSERT_EQ(actual.field_name().span(), expected.field_name().span());
        actual.base()->visit(*this, *expected.base());
    }

    void operator()(const loxmocha::expr::call_t& actual, const loxmocha::expr::call_t& expected)
    {
        actual.callee()->visit(*this, expected.callee());

        ASSERT_EQ(actual.positional_args().size(), expected.positional_args().size());
        for (size_t i = 0; i < actual.positional_args().size(); ++i) {
            actual.positional_args()[i].visit(*this, expected.positional_args()[i]);
        }

        ASSERT_EQ(actual.named_args().size(), expected.named_args().size());
        for (size_t i = 0; i < actual.named_args().size(); ++i) {
            ASSERT_EQ(actual.named_args()[i].name.kind(), loxmocha::token_t::kind_e::k_identifier);
            ASSERT_EQ(actual.named_args()[i].name.kind(), expected.named_args()[i].name.kind());
            ASSERT_EQ(actual.named_args()[i].name.span(), expected.named_args()[i].name.span());
            actual.named_args()[i].value.visit(*this, expected.named_args()[i].value);
        }
    }

    void operator()(const loxmocha::expr::if_t& actual, const loxmocha::expr::if_t& expected)
    {
        actual.condition()->visit(*this, *expected.condition());
        actual.then_branch()->visit(*this, *expected.then_branch());
        if (actual.else_branch() && expected.else_branch()) {
            actual.else_branch()->visit(*this, *expected.else_branch());
        } else {
            ASSERT_EQ(!!actual.else_branch(), !!expected.else_branch());
        }
    }

    void operator()(const loxmocha::expr::while_t& actual, const loxmocha::expr::while_t& expected)
    {
        actual.condition()->visit(*this, *expected.condition());
        actual.body()->visit(*this, *expected.body());
    }

    void operator()(const loxmocha::expr::block_t& actual, const loxmocha::expr::block_t& expected)
    {
        ASSERT_EQ(actual.statements().size(), expected.statements().size());
        for (size_t i = 0; i < actual.statements().size(); ++i) {
            actual.statements()[i].visit(*this, expected.statements()[i]);
        }
    }

    void operator()(const loxmocha::expr::grouping_t& actual, const loxmocha::expr::grouping_t& expected)
    {
        actual.expression()->visit(*this, *expected.expression());
    }
};

namespace {
auto make_expr(loxmocha::expr::expr_t&& expr) -> loxmocha::safe_ptr<loxmocha::expr::expr_t>
{
    return loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(std::move(expr));
}
} // namespace

TEST(ExprTest, ParserBinaryExprPrecedenceTest)
{
    using namespace loxmocha;

    const auto*       parse_string = "a + b * c - d / e";
    loxmocha::lexer_t lexer{parse_string};
    auto              result = loxmocha::parse_expr(lexer);

    ASSERT_TRUE(result);

    // We expect the following AST:
    // binary p_minus
    //     binary p_plus
    //         identifier a
    //         binary p_asterisk
    //             identifier b
    //             identifier c
    //     binary p_slash
    //         identifier d
    //         identifier e
    result.result().visit(
        expr_assert_visitor{},
        expr::binary_t{token_t::p_minus("-"),
                       make_expr(expr::binary_t{
                           token_t::p_plus("+"),
                           make_expr(expr::identifier_t{token_t::k_identifier("a")}),
                           make_expr(expr::binary_t{token_t::p_asterisk("*"),
                                                    make_expr(expr::identifier_t{token_t::k_identifier("b")}),
                                                    make_expr(expr::identifier_t{token_t::k_identifier("c")})})}),
                       make_expr(expr::binary_t{token_t::p_slash("/"),
                                                make_expr(expr::identifier_t{token_t::k_identifier("d")}),
                                                make_expr(expr::identifier_t{token_t::k_identifier("e")})})});
}