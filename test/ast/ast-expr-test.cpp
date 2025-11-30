#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"
#include <ranges>
#include <utility>
#include <vector>

class expr_assert_visitor {
public:
    void operator()([[maybe_unused]] const auto& actual, [[maybe_unused]] const auto& expected)
    {
        FAIL() << "Unexpected expression type encountered in assertion visitor.";
    }

    void operator()(const auto& actual, const loxmocha::expr::expr_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const loxmocha::expr::literal_t& actual, const loxmocha::expr::literal_t& expected)
    {
        EXPECT_TRUE(actual.value().kind() == loxmocha::token_t::kind_e::l_integer
                    || actual.value().kind() == loxmocha::token_t::kind_e::l_char
                    || actual.value().kind() == loxmocha::token_t::kind_e::l_string);

        EXPECT_EQ(actual.value().kind(), expected.value().kind());
        EXPECT_EQ(actual.value().span(), expected.value().span());
    }

    void operator()(const loxmocha::expr::identifier_t& actual, const loxmocha::expr::identifier_t& expected)
    {
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    void operator()(const loxmocha::expr::binary_t& actual, const loxmocha::expr::binary_t& expected)
    {
        EXPECT_EQ(actual.op().kind(), expected.op().kind());
        actual.left()->visit(*this, *expected.left());
        actual.right()->visit(*this, *expected.right());
    }

    void operator()(const loxmocha::expr::unary_t& actual, const loxmocha::expr::unary_t& expected)
    {
        EXPECT_EQ(actual.op().kind(), expected.op().kind());
        actual.operand()->visit(*this, *expected.operand());
    }

    void operator()(const loxmocha::expr::array_t& actual, const loxmocha::expr::array_t& expected)
    {
        ASSERT_EQ(actual.elements().size(), expected.elements().size());

        for (const auto& [a_elem, e_elem] : std::views::zip(actual.elements(), expected.elements())) {
            a_elem.visit(*this, e_elem);
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

        for (const auto& [a_elem, e_elem] : std::views::zip(actual.elements(), expected.elements())) {
            a_elem.visit(*this, e_elem);
        }
    }

    void operator()(const loxmocha::expr::record_t& actual, const loxmocha::expr::record_t& expected)
    {
        ASSERT_EQ(actual.fields().size(), expected.fields().size());

        for (const auto& [a_field, e_field] : std::views::zip(actual.fields(), expected.fields())) {
            EXPECT_EQ(a_field.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_field.name.kind(), e_field.name.kind());
            EXPECT_EQ(a_field.name.span(), e_field.name.span());
            a_field.value.visit(*this, e_field.value);
        }
    }

    void operator()(const loxmocha::expr::index_t& actual, const loxmocha::expr::index_t& expected)
    {
        actual.base()->visit(*this, *expected.base());
        actual.index()->visit(*this, *expected.index());
    }

    void operator()(const loxmocha::expr::field_t& actual, const loxmocha::expr::field_t& expected)
    {
        EXPECT_EQ(actual.field_name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.field_name().kind(), expected.field_name().kind());
        EXPECT_EQ(actual.field_name().span(), expected.field_name().span());
        actual.base()->visit(*this, *expected.base());
    }

    void operator()(const loxmocha::expr::call_t& actual, const loxmocha::expr::call_t& expected)
    {
        actual.callee()->visit(*this, *expected.callee());

        ASSERT_EQ(actual.positional_args().size(), expected.positional_args().size());

        for (const auto& [a_arg, e_arg] : std::views::zip(actual.positional_args(), expected.positional_args())) {
            a_arg.visit(*this, e_arg);
        }

        ASSERT_EQ(actual.named_args().size(), expected.named_args().size());
        for (const auto& [a_arg, e_arg] : std::views::zip(actual.named_args(), expected.named_args())) {
            EXPECT_EQ(a_arg.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_arg.name.kind(), e_arg.name.kind());
            EXPECT_EQ(a_arg.name.span(), e_arg.name.span());
            a_arg.value.visit(*this, e_arg.value);
        }
    }

    void operator()(const loxmocha::expr::if_t& actual, const loxmocha::expr::if_t& expected)
    {
        actual.condition()->visit(*this, *expected.condition());
        actual.then_branch()->visit(*this, *expected.then_branch());
        if (actual.else_branch() && expected.else_branch()) {
            actual.else_branch()->visit(*this, *expected.else_branch());
        } else {
            EXPECT_EQ(!!actual.else_branch(), !!expected.else_branch());
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

        for (const auto& [a_stmt, e_stmt] : std::views::zip(actual.statements(), expected.statements())) {
            a_stmt.visit(*this, e_stmt);
        }
    }

    void operator()(const loxmocha::expr::grouping_t& actual, const loxmocha::expr::grouping_t& expected)
    {
        actual.expression()->visit(*this, *expected.expression());
    }
};

namespace {
auto e(loxmocha::expr::expr_t&& expr) -> loxmocha::safe_ptr<loxmocha::expr::expr_t>
{
    return loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(std::move(expr));
}

template<typename T, typename... Args>
auto make_vector(Args&&... args) -> std::vector<T>
{
    std::vector<T> vec;
    vec.reserve(sizeof...(Args));
    (vec.emplace_back(std::forward<Args>(args)), ...);
    return vec;
}

} // namespace

TEST(ParserTest, ParserLiteralStringTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"("this is a string")"};
    auto    result = parse_expr(lexer);

    result.result().visit(expr_assert_visitor{}, expr::literal_t{token_t::l_string(R"("this is a string")")});
}

TEST(ParserTest, ParserLiteralCharTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"('c')"};
    auto    result = parse_expr(lexer);

    result.result().visit(expr_assert_visitor{}, expr::literal_t{token_t::l_char(R"('c')")});
}

TEST(ParserTest, ParserLiteralIntegerTest)
{
    using namespace loxmocha;
    lexer_t lexer{"12345"};
    auto    result = parse_expr(lexer);

    result.result().visit(expr_assert_visitor{}, expr::literal_t{token_t::l_integer("12345")});
}

TEST(ParserTest, ParserIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"my_variable"};
    auto    result = parse_expr(lexer);

    result.result().visit(expr_assert_visitor{}, expr::identifier_t{token_t::k_identifier("my_variable")});
}

TEST(ParserTest, ParserEqualityTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a == b"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_equal_equal("=="),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x != y"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_not_equal("!="),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x != y == z"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_equal_equal("=="),
                           e(expr::binary_t{token_t::p_not_equal("!="),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }
}

TEST(ParserTest, ParserNoRHSEqualityTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a =="};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }
}

TEST(ParserTest, ParserComparisonTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a < b"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_less("<"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x <= y"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_less_equal("<="),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x > y >= z"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_greater_equal(">="),
                           e(expr::binary_t{token_t::p_greater(">"),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }

    {
        lexer_t lexer{"a < b == c > d"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_equal_equal("=="),
                           e(expr::binary_t{token_t::p_less("<"),
                                            e(expr::identifier_t{token_t::k_identifier("a")}),
                                            e(expr::identifier_t{token_t::k_identifier("b")})}),
                           e(expr::binary_t{token_t::p_greater(">"),
                                            e(expr::identifier_t{token_t::k_identifier("c")}),
                                            e(expr::identifier_t{token_t::k_identifier("d")})})});
    }

    {
        lexer_t lexer{"a >"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }

    {
        lexer_t lexer{"x <"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }
}

TEST(ParserTest, ParserNoRHSComparisonTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a >"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }
}

TEST(ParserTest, ParserAdditionTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a + b"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_plus("+"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x - y"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_minus("-"),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x - y + z"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_plus("+"),
                           e(expr::binary_t{token_t::p_minus("-"),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }

    {
        lexer_t lexer{"a + b < c - d"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_less("<"),
                           e(expr::binary_t{token_t::p_plus("+"),
                                            e(expr::identifier_t{token_t::k_identifier("a")}),
                                            e(expr::identifier_t{token_t::k_identifier("b")})}),
                           e(expr::binary_t{token_t::p_minus("-"),
                                            e(expr::identifier_t{token_t::k_identifier("c")}),
                                            e(expr::identifier_t{token_t::k_identifier("d")})})});
    }
}

TEST(ParserTest, ParserNoRHSAdditionTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a +"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }
}

TEST(ParserTest, ParserMultiplicationTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a * b"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_asterisk("*"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x / y"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::binary_t{token_t::p_slash("/"),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x / y * z"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_asterisk("*"),
                           e(expr::binary_t{token_t::p_slash("/"),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }

    {
        lexer_t lexer{"a * b + c / d + e * f"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_plus("+"),
                           e(expr::binary_t{token_t::p_plus("+"),
                                            e(expr::binary_t{token_t::p_asterisk("*"),
                                                             e(expr::identifier_t{token_t::k_identifier("a")}),
                                                             e(expr::identifier_t{token_t::k_identifier("b")})}),
                                            e(expr::binary_t{token_t::p_slash("/"),
                                                             e(expr::identifier_t{token_t::k_identifier("c")}),
                                                             e(expr::identifier_t{token_t::k_identifier("d")})})}),
                           e(expr::binary_t{token_t::p_asterisk("*"),
                                            e(expr::identifier_t{token_t::k_identifier("e")}),
                                            e(expr::identifier_t{token_t::k_identifier("f")})})});
    }
}

TEST(ParserTest, ParserNoRHSMultiplicationTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a *"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }
}

TEST(ParserTest, ParserUnaryTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"-a"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})});
    }

    {
        lexer_t lexer{"!c"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("c")})});
    }

    {
        lexer_t lexer{"-!b"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::unary_t{token_t::p_minus("-"),
                          e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("b")})})});
    }

    {
        lexer_t lexer{"!x == y"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_equal_equal("=="),
                           e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("x")})}),
                           e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"-a + b * !c + -d / e"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{
                token_t::p_plus("+"),
                e(expr::binary_t{
                    token_t::p_plus("+"),
                    e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                    e(expr::binary_t{
                        token_t::p_asterisk("*"),
                        e(expr::identifier_t{token_t::k_identifier("b")}),
                        e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("c")})})})}),
                e(expr::binary_t{
                    token_t::p_slash("/"),
                    e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("d")})}),
                    e(expr::identifier_t{token_t::k_identifier("e")})})});
    }
}

TEST(ParserTest, ParserFieldAccessTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a.b"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")});
    }

    {
        lexer_t lexer{"a.b.c"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::field_t{
                e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")}),
                token_t::k_identifier("c")});
    }

    {
        lexer_t lexer{"a.b.c + 2"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_plus("+"),
                           e(expr::field_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                                           token_t::k_identifier("b")}),
                                           token_t::k_identifier("c")}),
                           e(expr::literal_t{token_t::l_integer("2")})});
    }
}

TEST(ParserTest, ParserFieldAccessNoIdentifierTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"obj."};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Expected identifier after '.'");
    }

    {
        lexer_t lexer{"obj.2"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Expected identifier after '.'");
    }

    {
        lexer_t lexer{"obj.if"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Expected identifier after '.'");
    }
}

TEST(ParserTest, ParserIndexAccessTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"arr[0]"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::index_t{e(expr::identifier_t{token_t::k_identifier("arr")}),
                                                       e(expr::literal_t{token_t::l_integer("0")})});
    }

    {
        lexer_t lexer{"matrix[i][j]"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::index_t{e(expr::index_t{e(expr::identifier_t{token_t::k_identifier("matrix")}),
                                          e(expr::identifier_t{token_t::k_identifier("i")})}),
                          e(expr::identifier_t{token_t::k_identifier("j")})});
    }

    {
        lexer_t lexer{"arr[i + 2] * 3"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_asterisk("*"),
                           e(expr::index_t{e(expr::identifier_t{token_t::k_identifier("arr")}),
                                           e(expr::binary_t{token_t::p_plus("+"),
                                                            e(expr::identifier_t{token_t::k_identifier("i")}),
                                                            e(expr::literal_t{token_t::l_integer("2")})})}),
                           e(expr::literal_t{token_t::l_integer("3")})});
    }
}

TEST(ParserTest, ParserIndexAccessNoExpressionTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"arr[2)"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Expected ']' after index expression");
    }
}

TEST(ParserTest, ParserCallTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"foo()"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::call_t{e(expr::identifier_t{token_t::k_identifier("foo")}), {}, {}});
    }

    {
        lexer_t lexer{"foo(2)"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::call_t{e(expr::identifier_t{token_t::k_identifier("foo")}),
                         make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("2")}),
                         {}});
    }

    {
        lexer_t lexer{"sum(a, b, 1 + 2, c * 3)"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::call_t{e(expr::identifier_t{token_t::k_identifier("sum")}),
                         make_vector<expr::expr_t>(expr::identifier_t{token_t::k_identifier("a")},
                                                   expr::identifier_t{token_t::k_identifier("b")},
                                                   expr::binary_t{token_t::p_plus("+"),
                                                                  e(expr::literal_t{token_t::l_integer("1")}),
                                                                  e(expr::literal_t{token_t::l_integer("2")})},
                                                   expr::binary_t{token_t::p_asterisk("*"),
                                                                  e(expr::identifier_t{token_t::k_identifier("c")}),
                                                                  e(expr::literal_t{token_t::l_integer("3")})}),
                         {}});
    }

    {
        lexer_t lexer{R"(print(message: "Hello", count: 5))"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::call_t{e(expr::identifier_t{token_t::k_identifier("print")}),
                         {},
                         make_vector<expr::call_t::named_arg_t>(
                             expr::call_t::named_arg_t{.name  = token_t::k_identifier("message"),
                                                       .value = expr::literal_t{token_t::l_string(R"("Hello")")}},
                             expr::call_t::named_arg_t{.name  = token_t::k_identifier("count"),
                                                       .value = expr::literal_t{token_t::l_integer("5")}})});
    }

    {
        lexer_t lexer{"lerp(a, b, t: 0)"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::call_t{e(expr::identifier_t{token_t::k_identifier("lerp")}),
                         make_vector<expr::expr_t>(expr::identifier_t{token_t::k_identifier("a")},
                                                   expr::identifier_t{token_t::k_identifier("b")}),
                         make_vector<expr::call_t::named_arg_t>(expr::call_t::named_arg_t{
                             .name = token_t::k_identifier("t"), .value = expr::literal_t{token_t::l_integer("0")}})});
    }
}

TEST(ParserTest, ParserPositionalAfterNamedArgsTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"func(arg1: 10, 20)"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 2);
        ASSERT_EQ(result.diagnostics().front(), "Expected identifier for named argument");
        ASSERT_EQ(result.diagnostics().back(), "Expected ')' after arguments");
    }

    {
        lexer_t lexer("func(a, b: 2, c)");
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        ASSERT_EQ(result.diagnostics().front(), "Expected ':' after named argument identifier");
    }
}

TEST(ParserTest, ParserCallNoClosingParenTest)
{
    using namespace loxmocha;

    lexer_t lexer{"foo(2, 3]"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 2);
    EXPECT_EQ(result.diagnostics().front(), "Expected identifier for named argument");
    EXPECT_EQ(result.diagnostics().back(), "Expected ')' after arguments");
}

TEST(ParserTest, ParserMixedAccessTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"obj.field[index](arg1, arg2).anotherField"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::field_t{
                e(expr::call_t{e(expr::index_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("obj")}),
                                                               token_t::k_identifier("field")}),
                                               e(expr::identifier_t{token_t::k_identifier("index")})}),
                               make_vector<expr::expr_t>(expr::identifier_t{token_t::k_identifier("arg1")},
                                                         expr::identifier_t{token_t::k_identifier("arg2")}),
                               {}}),
                token_t::k_identifier("anotherField")});
    }
}

TEST(ParserTest, ParserGroupingTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"(a)"};
        parse_expr(lexer).result().visit(expr_assert_visitor{},
                                         expr::grouping_t{e(expr::identifier_t{token_t::k_identifier("a")})});
    }

    {
        lexer_t lexer{"(x + y)"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::grouping_t{e(expr::binary_t{token_t::p_plus("+"),
                                              e(expr::identifier_t{token_t::k_identifier("x")}),
                                              e(expr::identifier_t{token_t::k_identifier("y")})})});
    }

    {
        lexer_t lexer{"(a + b) * (c - d)"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_asterisk("*"),
                           e(expr::grouping_t{e(expr::binary_t{token_t::p_plus("+"),
                                                               e(expr::identifier_t{token_t::k_identifier("a")}),
                                                               e(expr::identifier_t{token_t::k_identifier("b")})})}),
                           e(expr::grouping_t{e(expr::binary_t{token_t::p_minus("-"),
                                                               e(expr::identifier_t{token_t::k_identifier("c")}),
                                                               e(expr::identifier_t{token_t::k_identifier("d")})})})});
    }

    {
        lexer_t lexer{"(call(a, b) + 3) / 2"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_slash("/"),
                           e(expr::grouping_t{e(expr::binary_t{
                               token_t::p_plus("+"),
                               e(expr::call_t{e(expr::identifier_t{token_t::k_identifier("call")}),
                                              make_vector<expr::expr_t>(expr::identifier_t{token_t::k_identifier("a")},
                                                                        expr::identifier_t{token_t::k_identifier("b")}),
                                              {}}),
                               e(expr::literal_t{token_t::l_integer("3")})})}),
                           e(expr::literal_t{token_t::l_integer("2")})});
    }

    {
        lexer_t lexer{"(a + b) * c"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::binary_t{token_t::p_asterisk("*"),
                           e(expr::grouping_t{e(expr::binary_t{token_t::p_plus("+"),
                                                               e(expr::identifier_t{token_t::k_identifier("a")}),
                                                               e(expr::identifier_t{token_t::k_identifier("b")})})}),
                           e(expr::identifier_t{token_t::k_identifier("c")})});
    }

    {
        lexer_t lexer{"-(x + y)"};
        parse_expr(lexer).result().visit(
            expr_assert_visitor{},
            expr::unary_t{token_t::p_minus("-"),
                          e(expr::grouping_t{e(expr::binary_t{token_t::p_plus("+"),
                                                              e(expr::identifier_t{token_t::k_identifier("x")}),
                                                              e(expr::identifier_t{token_t::k_identifier("y")})})})});
    }
}

TEST(ParserTest, ParserGroupingNoClosingParenTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"(a + b]"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Expected ')' after expression");
    }
}

TEST(ParserTest, ParserEmptyInputTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{""};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 1);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
    }
}

TEST(ParserTest, ParserInvalidTokenTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"+"};
        auto    result = parse_expr(lexer);

        ASSERT_TRUE(!result);
        ASSERT_EQ(result.diagnostics().size(), 2);
        EXPECT_EQ(result.diagnostics().front(), "Unexpected token: +");
        EXPECT_EQ(result.diagnostics().back(), "Unexpected end of input");
    }
}
