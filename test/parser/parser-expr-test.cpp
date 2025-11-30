#include "assert_visitor.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"
#include <utility>
#include <vector>

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

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::l_string(R"("this is a string")")});
}

TEST(ParserTest, ParserLiteralCharTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"('c')"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::l_char(R"('c')")});
}

TEST(ParserTest, ParserLiteralIntegerTest)
{
    using namespace loxmocha;
    lexer_t lexer{"12345"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::l_integer("12345")});
}

TEST(ParserTest, ParserIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"my_variable"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::identifier_t{token_t::k_identifier("my_variable")});
}

TEST(ParserTest, ParserEqualityTest)
{
    using namespace loxmocha;

    {
        lexer_t lexer{"a == b"};
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_equal_equal("=="),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x != y"};
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_not_equal("!="),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x != y == z"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_less("<"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x <= y"};
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_less_equal("<="),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x > y >= z"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::binary_t{token_t::p_greater_equal(">="),
                           e(expr::binary_t{token_t::p_greater(">"),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }

    {
        lexer_t lexer{"a < b == c > d"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_plus("+"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x - y"};
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_minus("-"),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x - y + z"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::binary_t{token_t::p_plus("+"),
                           e(expr::binary_t{token_t::p_minus("-"),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }

    {
        lexer_t lexer{"a + b < c - d"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_asterisk("*"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})});
    }

    {
        lexer_t lexer{"x / y"};
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::binary_t{token_t::p_slash("/"),
                                                        e(expr::identifier_t{token_t::k_identifier("x")}),
                                                        e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"x / y * z"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::binary_t{token_t::p_asterisk("*"),
                           e(expr::binary_t{token_t::p_slash("/"),
                                            e(expr::identifier_t{token_t::k_identifier("x")}),
                                            e(expr::identifier_t{token_t::k_identifier("y")})}),
                           e(expr::identifier_t{token_t::k_identifier("z")})});
    }

    {
        lexer_t lexer{"a * b + c / d + e * f"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
            test::assert_visitor{},
            expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})});
    }

    {
        lexer_t lexer{"!c"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("c")})});
    }

    {
        lexer_t lexer{"-!b"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::unary_t{token_t::p_minus("-"),
                          e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("b")})})});
    }

    {
        lexer_t lexer{"!x == y"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::binary_t{token_t::p_equal_equal("=="),
                           e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("x")})}),
                           e(expr::identifier_t{token_t::k_identifier("y")})});
    }

    {
        lexer_t lexer{"-a + b * !c + -d / e"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
            test::assert_visitor{},
            expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")});
    }

    {
        lexer_t lexer{"a.b.c"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::field_t{
                e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")}),
                token_t::k_identifier("c")});
    }

    {
        lexer_t lexer{"a.b.c + 2"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::index_t{e(expr::identifier_t{token_t::k_identifier("arr")}),
                                                       e(expr::literal_t{token_t::l_integer("0")})});
    }

    {
        lexer_t lexer{"matrix[i][j]"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::index_t{e(expr::index_t{e(expr::identifier_t{token_t::k_identifier("matrix")}),
                                          e(expr::identifier_t{token_t::k_identifier("i")})}),
                          e(expr::identifier_t{token_t::k_identifier("j")})});
    }

    {
        lexer_t lexer{"arr[i + 2] * 3"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::call_t{e(expr::identifier_t{token_t::k_identifier("foo")}), {}, {}});
    }

    {
        lexer_t lexer{"foo(2)"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::call_t{e(expr::identifier_t{token_t::k_identifier("foo")}),
                         make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("2")}),
                         {}});
    }

    {
        lexer_t lexer{"sum(a, b, 1 + 2, c * 3)"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
            test::assert_visitor{},
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
            test::assert_visitor{},
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
            test::assert_visitor{},
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
        parse_expr(lexer).result().visit(test::assert_visitor{},
                                         expr::grouping_t{e(expr::identifier_t{token_t::k_identifier("a")})});
    }

    {
        lexer_t lexer{"(x + y)"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
            expr::grouping_t{e(expr::binary_t{token_t::p_plus("+"),
                                              e(expr::identifier_t{token_t::k_identifier("x")}),
                                              e(expr::identifier_t{token_t::k_identifier("y")})})});
    }

    {
        lexer_t lexer{"(a + b) * (c - d)"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
            test::assert_visitor{},
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
            test::assert_visitor{},
            expr::binary_t{token_t::p_asterisk("*"),
                           e(expr::grouping_t{e(expr::binary_t{token_t::p_plus("+"),
                                                               e(expr::identifier_t{token_t::k_identifier("a")}),
                                                               e(expr::identifier_t{token_t::k_identifier("b")})})}),
                           e(expr::identifier_t{token_t::k_identifier("c")})});
    }

    {
        lexer_t lexer{"-(x + y)"};
        parse_expr(lexer).result().visit(
            test::assert_visitor{},
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
