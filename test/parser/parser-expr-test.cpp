#include "assert_visitor.hpp"
#include "helpers.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <vector>

using namespace loxmocha::test::helpers;

TEST(ParserTest, ExprLiteralStringTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"("this is a string")"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::l_string(R"("this is a string")")});
}

TEST(ParserTest, ExprLiteralCharTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"('c')"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::l_char(R"('c')")});
}

TEST(ParserTest, ExprLiteralIntegerTest)
{
    using namespace loxmocha;
    lexer_t lexer{"12345"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::l_integer("12345")});
}

TEST(ParserTest, ExprIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"my_variable"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::identifier_t{token_t::k_identifier("my_variable")});
}

TEST(ParserTest, ExprLiteralTrueTest)
{
    using namespace loxmocha;
    lexer_t lexer{"true"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::k_true("true")});
}

TEST(ParserTest, ExprLiteralFalseTest)
{
    using namespace loxmocha;
    lexer_t lexer{"false"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::literal_t{token_t::k_false("false")});
}

TEST(ParserTest, ExprAndTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a && b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_and_and("||"),
                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                    e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprOrTest)
{
    using namespace loxmocha;
    lexer_t lexer{"x || y"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_pipe_pipe("||"),
                                                    e(expr::identifier_t{token_t::k_identifier("x")}),
                                                    e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleLogicalTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a && b || c && d"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_pipe_pipe("||"),
                       e(expr::binary_t{token_t::p_and_and("&&"),
                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                        e(expr::identifier_t{token_t::k_identifier("b")})}),
                       e(expr::binary_t{token_t::p_and_and("&&"),
                                        e(expr::identifier_t{token_t::k_identifier("c")}),
                                        e(expr::identifier_t{token_t::k_identifier("d")})})});
}

TEST(ParserTest, ExprEqualityTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a == b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_equal_equal("=="),
                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                    e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprInequalityTest)
{
    using namespace loxmocha;
    lexer_t lexer{"x != y"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_not_equal("!="),
                                                    e(expr::identifier_t{token_t::k_identifier("x")}),
                                                    e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleEqualityTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a == b != c == d != e"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{
            token_t::p_not_equal("!="),
            e(expr::binary_t{token_t::p_equal_equal("=="),
                             e(expr::binary_t{token_t::p_not_equal("!="),
                                              e(expr::binary_t{token_t::p_equal_equal("=="),
                                                               e(expr::identifier_t{token_t::k_identifier("a")}),
                                                               e(expr::identifier_t{token_t::k_identifier("b")})}),
                                              e(expr::identifier_t{token_t::k_identifier("c")})}),
                             e(expr::identifier_t{token_t::k_identifier("d")})}),
            e(expr::identifier_t{token_t::k_identifier("e")})});
}

TEST(ParserTest, ExprEqualityPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a == b && c != d"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_and_and("&&"),
                       e(expr::binary_t{token_t::p_equal_equal("=="),
                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                        e(expr::identifier_t{token_t::k_identifier("b")})}),
                       e(expr::binary_t{token_t::p_not_equal("!="),
                                        e(expr::identifier_t{token_t::k_identifier("c")}),
                                        e(expr::identifier_t{token_t::k_identifier("d")})})});
}

TEST(ParserTest, ExprLessThanTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a < b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_less("<"),
                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                    e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprLessThanOrEqualTest)
{
    using namespace loxmocha;
    lexer_t lexer{"x <= y"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_less_equal("<="),
                                                    e(expr::identifier_t{token_t::k_identifier("x")}),
                                                    e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprGreaterThanTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a > b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_greater(">"),
                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                    e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprGreaterThanOrEqualTest)
{
    using namespace loxmocha;
    lexer_t lexer{"x >= y"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_greater_equal(">="),
                                                    e(expr::identifier_t{token_t::k_identifier("x")}),
                                                    e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleComparisonTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a < b <= c > d >= e"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{
            token_t::p_greater_equal(">="),
            e(expr::binary_t{token_t::p_greater(">"),
                             e(expr::binary_t{token_t::p_less_equal("<="),
                                              e(expr::binary_t{token_t::p_less("<"),
                                                               e(expr::identifier_t{token_t::k_identifier("a")}),
                                                               e(expr::identifier_t{token_t::k_identifier("b")})}),
                                              e(expr::identifier_t{token_t::k_identifier("c")})}),
                             e(expr::identifier_t{token_t::k_identifier("d")})}),
            e(expr::identifier_t{token_t::k_identifier("e")})});
}

TEST(ParserTest, ExprComparisonPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a < b == b > c"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_equal_equal("=="),
                       e(expr::binary_t{token_t::p_less("<"),
                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                        e(expr::identifier_t{token_t::k_identifier("b")})}),
                       e(expr::binary_t{token_t::p_greater(">"),
                                        e(expr::identifier_t{token_t::k_identifier("b")}),
                                        e(expr::identifier_t{token_t::k_identifier("c")})})});
}

TEST(ParserTest, ExprAdditionTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a + b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_plus("+"),
                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                    e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprSubtractionTest)
{
    using namespace loxmocha;
    lexer_t lexer{"x - y"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_minus("-"),
                                                    e(expr::identifier_t{token_t::k_identifier("x")}),
                                                    e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleAdditionTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a + b - c + d"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_plus("+"),
                       e(expr::binary_t{token_t::p_minus("-"),
                                        e(expr::binary_t{token_t::p_plus("+"),
                                                         e(expr::identifier_t{token_t::k_identifier("a")}),
                                                         e(expr::identifier_t{token_t::k_identifier("b")})}),
                                        e(expr::identifier_t{token_t::k_identifier("c")})}),
                       e(expr::identifier_t{token_t::k_identifier("d")})});
}

TEST(ParserTest, ExprAdditionPrecedenceTest)
{
    using namespace loxmocha;
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

TEST(ParserTest, ExprMultiplicationTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a * b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_asterisk("*"),
                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                    e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprDivisionTest)
{
    using namespace loxmocha;
    lexer_t lexer{"x / y"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::binary_t{token_t::p_slash("/"),
                                                    e(expr::identifier_t{token_t::k_identifier("x")}),
                                                    e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleMultiplicationTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a * b / c * d"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_asterisk("*"),
                       e(expr::binary_t{token_t::p_slash("/"),
                                        e(expr::binary_t{token_t::p_asterisk("*"),
                                                         e(expr::identifier_t{token_t::k_identifier("a")}),
                                                         e(expr::identifier_t{token_t::k_identifier("b")})}),
                                        e(expr::identifier_t{token_t::k_identifier("c")})}),
                       e(expr::identifier_t{token_t::k_identifier("d")})});
}

TEST(ParserTest, ExprMultiplicationPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a * b + c / d"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_plus("+"),
                       e(expr::binary_t{token_t::p_asterisk("*"),
                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                        e(expr::identifier_t{token_t::k_identifier("b")})}),
                       e(expr::binary_t{token_t::p_slash("/"),
                                        e(expr::identifier_t{token_t::k_identifier("c")}),
                                        e(expr::identifier_t{token_t::k_identifier("d")})})});
}

TEST(ParserTest, ExprNegationTest)
{
    using namespace loxmocha;
    lexer_t lexer{"-42"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{}, expr::unary_t{token_t::p_minus("-"), e(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, ExprLogicalNotTest)
{
    using namespace loxmocha;
    lexer_t lexer{"!true"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::unary_t{token_t::p_bang("!"), e(expr::literal_t{token_t::k_true("true")})});
}

TEST(ParserTest, ExprMultipleUnaryTest)
{
    using namespace loxmocha;
    lexer_t lexer{"-!false"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::unary_t{token_t::p_minus("-"),
                      e(expr::unary_t{token_t::p_bang("!"), e(expr::literal_t{token_t::k_false("false")})})});
}

TEST(ParserTest, ExprUnaryPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"-a * !b"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_asterisk("*"),
                       e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                       e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("b")})})});
}

TEST(ParserTest, ExprFieldAccessTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a.b.c"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::field_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")}),
                      token_t::k_identifier("c")});
}

TEST(ParserTest, ExprIndexAccessTest)
{
    using namespace loxmocha;
    lexer_t lexer{"matrix[i + 2][j]"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::index_t{e(expr::index_t{e(expr::identifier_t{token_t::k_identifier("matrix")}),
                                      e(expr::binary_t{token_t::p_plus("+"),
                                                       e(expr::identifier_t{token_t::k_identifier("i")}),
                                                       e(expr::literal_t{token_t::l_integer("2")})})}),
                      e(expr::identifier_t{token_t::k_identifier("j")})});
}

TEST(ParserTest, ExprCallNoArgumentsTest)
{
    using namespace loxmocha;
    lexer_t lexer{"func()"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::call_t{e(expr::identifier_t{token_t::k_identifier("func")}), {}, {}});
}

TEST(ParserTest, ExprCallPositionalArgumentsTest)
{
    using namespace loxmocha;
    lexer_t lexer{"add(5, 10)"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::call_t{e(expr::identifier_t{token_t::k_identifier("add")}),
                                                  make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("5")},
                                                                            expr::literal_t{token_t::l_integer("10")}),
                                                  {}});
}

TEST(ParserTest, ExprCallNamedArgumentsTest)
{
    using namespace loxmocha;
    lexer_t lexer{"setPosition(x: 100, y: 200)"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::call_t{e(expr::identifier_t{token_t::k_identifier("setPosition")}),
                     {},
                     make_vector<expr::call_t::named_arg_t>(
                         expr::call_t::named_arg_t{.name  = token_t::k_identifier("x"),
                                                   .value = expr::literal_t{token_t::l_integer("100")}},
                         expr::call_t::named_arg_t{.name  = token_t::k_identifier("y"),
                                                   .value = expr::literal_t{token_t::l_integer("200")}})});
}

TEST(ParserTest, ExprCallMixedArgumentTest)
{
    using namespace loxmocha;
    lexer_t lexer{"drawCircle(x + 10, y, radius: 25)"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::call_t{
            e(expr::identifier_t{token_t::k_identifier("drawCircle")}),
            make_vector<expr::expr_t>(expr::binary_t{token_t::p_plus("+"),
                                                     e(expr::identifier_t{token_t::k_identifier("x")}),
                                                     e(expr::literal_t{token_t::l_integer("10")})},
                                      expr::identifier_t{token_t::k_identifier("y")}),
            make_vector<expr::call_t::named_arg_t>(expr::call_t::named_arg_t{
                .name = token_t::k_identifier("radius"), .value = expr::literal_t{token_t::l_integer("25")}})});
}

TEST(ParserTest, ExprPositionalAfterNamedArgsTest)
{
    using namespace loxmocha;
    lexer_t lexer("func(a, b: 2, c)");
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    ASSERT_EQ(result.diagnostics().front(), "Expected ':' after named argument identifier");
}

TEST(ParserTest, ExprNestedCallsTest)
{
    using namespace loxmocha;
    lexer_t lexer{"outer(inner(1, 2), another(3))"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::call_t{
            e(expr::identifier_t{token_t::k_identifier("outer")}),
            make_vector<expr::expr_t>(expr::call_t{e(expr::identifier_t{token_t::k_identifier("inner")}),
                                                   make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                             expr::literal_t{token_t::l_integer("2")}),
                                                   {}},
                                      expr::call_t{e(expr::identifier_t{token_t::k_identifier("another")}),
                                                   make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("3")}),
                                                   {}}),
            {}});
}

TEST(ParserTest, ExprMixedAccessTest)
{
    using namespace loxmocha;
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

TEST(ParserTest, ExprAccessPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"-a.b + c.d[e.f] - -g.h()"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{
            token_t::p_minus("-"),
            e(expr::binary_t{
                token_t::p_plus("+"),
                e(expr::unary_t{
                    token_t::p_minus("-"),
                    e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")})}),
                e(expr::index_t{
                    e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("c")}), token_t::k_identifier("d")}),
                    e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("e")}), token_t::k_identifier("f")})})}),
            e(expr::unary_t{token_t::p_minus("-"),
                            e(expr::call_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("g")}),
                                                           token_t::k_identifier("h")}),
                                           {},
                                           {}})})});
}

TEST(ParserTest, ExprGroupingTest)
{
    using namespace loxmocha;
    lexer_t lexer{"(a)"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::grouping_t{e(expr::identifier_t{token_t::k_identifier("a")})});
}

TEST(ParserTest, ExprIsTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a is b"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::is_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                                p(pattern::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprIsPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"!a is b"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::is_t{e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                   p(pattern::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprIsPrecedenceWithGroupingTest)
{
    using namespace loxmocha;
    lexer_t lexer{"!(a is b)"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::unary_t{token_t::p_bang("!"),
                      e(expr::grouping_t{e(expr::is_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                                      p(pattern::identifier_t{token_t::k_identifier("b")})})})});
}

TEST(ParserTest, ExprIsPrecedenceWithAccessTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a.b is c"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::is_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")}),
                   p(pattern::identifier_t{token_t::k_identifier("c")})});
}

TEST(ParserTest, ExprIsPrecedenceWithAndTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a && b is c"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_and_and("&&"),
                       e(expr::identifier_t{token_t::k_identifier("a")}),
                       e(expr::is_t{e(expr::identifier_t{token_t::k_identifier("b")}),
                                    p(pattern::identifier_t{token_t::k_identifier("c")})})});
}

TEST(ParserTest, ExprCastTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a as MyType"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::cast_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                                  t(type::identifier_t{token_t::k_identifier("MyType")})});
}

TEST(ParserTest, ExprCastPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"-a as MyType"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::cast_t{e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                     t(type::identifier_t{token_t::k_identifier("MyType")})});
}

TEST(ParserTest, ExprArrayTest)
{
    using namespace loxmocha;
    lexer_t lexer{"[1, 2, 3]"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                  expr::literal_t{token_t::l_integer("2")},
                                                                  expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprArrayEmptyTest)
{
    using namespace loxmocha;
    lexer_t lexer{"[]"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::array_t{{}});
}

TEST(ParserTest, ExprArraySingleElementTest)
{
    using namespace loxmocha;
    lexer_t lexer{"[42]"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, ExprArrayTrailingCommaTest)
{
    using namespace loxmocha;
    lexer_t lexer{"[1, 2, 3, ]"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                  expr::literal_t{token_t::l_integer("2")},
                                                                  expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprArrayPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"[a + b, c * d][i - 1]"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::index_t{e(expr::array_t{make_vector<expr::expr_t>(
                          expr::binary_t{token_t::p_plus("+"),
                                         e(expr::identifier_t{token_t::k_identifier("a")}),
                                         e(expr::identifier_t{token_t::k_identifier("b")})},
                          expr::binary_t{token_t::p_asterisk("*"),
                                         e(expr::identifier_t{token_t::k_identifier("c")}),
                                         e(expr::identifier_t{token_t::k_identifier("d")})})}),
                      e(expr::binary_t{token_t::p_minus("-"),
                                       e(expr::identifier_t{token_t::k_identifier("i")}),
                                       e(expr::literal_t{token_t::l_integer("1")})})});
}

TEST(ParserTest, ExprRecordTest)
{
    using namespace loxmocha;
    lexer_t lexer{"{x: 10, y: 20}"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::record_t{make_vector<expr::record_t::field_t>(
                              expr::record_t::field_t{.name  = token_t::k_identifier("x"),
                                                      .value = expr::literal_t{token_t::l_integer("10")}},
                              expr::record_t::field_t{.name  = token_t::k_identifier("y"),
                                                      .value = expr::literal_t{token_t::l_integer("20")}})});
}

TEST(ParserTest, ExprRecordEmptyTest)
{
    using namespace loxmocha;
    lexer_t lexer{"{}"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::record_t{{}});
}

TEST(ParserTest, ExprRecordSingleFieldTest)
{
    using namespace loxmocha;
    lexer_t lexer{"{value: 42}"};
    auto    result = parse_expr(lexer);

    result.result().visit(
        test::assert_visitor{},
        expr::record_t{make_vector<expr::record_t::field_t>(expr::record_t::field_t{
            .name = token_t::k_identifier("value"), .value = expr::literal_t{token_t::l_integer("42")}})});
}

TEST(ParserTest, ExprRecordTrailingCommaTest)
{
    using namespace loxmocha;
    lexer_t lexer{"{a: 1, b: 2, }"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::record_t{make_vector<expr::record_t::field_t>(
                              expr::record_t::field_t{.name  = token_t::k_identifier("a"),
                                                      .value = expr::literal_t{token_t::l_integer("1")}},
                              expr::record_t::field_t{.name  = token_t::k_identifier("b"),
                                                      .value = expr::literal_t{token_t::l_integer("2")}})});
}

TEST(ParserTest, ExprRecordPrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"{sum: a + b, product: c * d}.sum"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::field_t{
            e(expr::record_t{make_vector<expr::record_t::field_t>(
                expr::record_t::field_t{.name  = token_t::k_identifier("sum"),
                                        .value = expr::binary_t{token_t::p_plus("+"),
                                                                e(expr::identifier_t{token_t::k_identifier("a")}),
                                                                e(expr::identifier_t{token_t::k_identifier("b")})}},
                expr::record_t::field_t{.name  = token_t::k_identifier("product"),
                                        .value = expr::binary_t{token_t::p_asterisk("*"),
                                                                e(expr::identifier_t{token_t::k_identifier("c")}),
                                                                e(expr::identifier_t{token_t::k_identifier("d")})}})}),
            token_t::k_identifier("sum")});
}

TEST(ParserTest, ExprTupleTest)
{
    using namespace loxmocha;
    lexer_t lexer{"(1, 2, 3)"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                  expr::literal_t{token_t::l_integer("2")},
                                                                  expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprTupleEmptyTest)
{
    using namespace loxmocha;
    lexer_t lexer{"()"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{}, expr::tuple_t{{}});
}

TEST(ParserTest, ExprTupleSingleElementTest)
{
    using namespace loxmocha;
    lexer_t lexer{"(42,)"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, ExprTupleTrailingCommaTest)
{
    using namespace loxmocha;
    lexer_t lexer{"(1, 2, 3, )"};
    auto    result = parse_expr(lexer);

    result.result().visit(test::assert_visitor{},
                          expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                  expr::literal_t{token_t::l_integer("2")},
                                                                  expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprTuplePrecedenceTest)
{
    using namespace loxmocha;
    lexer_t lexer{"(a + b, c * d)[1]"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::index_t{e(expr::tuple_t{make_vector<expr::expr_t>(
                          expr::binary_t{token_t::p_plus("+"),
                                         e(expr::identifier_t{token_t::k_identifier("a")}),
                                         e(expr::identifier_t{token_t::k_identifier("b")})},
                          expr::binary_t{token_t::p_asterisk("*"),
                                         e(expr::identifier_t{token_t::k_identifier("c")}),
                                         e(expr::identifier_t{token_t::k_identifier("d")})})}),
                      e(expr::literal_t{token_t::l_integer("1")})});
}

TEST(ParserTest, ExprIfTest)
{
    using namespace loxmocha;
    lexer_t lexer{"if a && b => c"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
            .condition   = e(expr::binary_t{token_t::p_and_and("&&"),
                                          e(expr::identifier_t{token_t::k_identifier("a")}),
                                          e(expr::identifier_t{token_t::k_identifier("b")})}),
            .then_branch = e(expr::identifier_t{token_t::k_identifier("c")})})});
}

TEST(ParserTest, ExprIfElseIfTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        if x < 10 => "small"
        else if x < 20 => "medium"
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(
            expr::if_t::conditional_branch_t{.condition =
                                                 e(expr::binary_t{token_t::p_less("<"),
                                                                  e(expr::identifier_t{token_t::k_identifier("x")}),
                                                                  e(expr::literal_t{token_t::l_integer("10")})}),
                                             .then_branch = e(expr::literal_t{token_t::l_string("\"small\"")})},
            expr::if_t::conditional_branch_t{.condition =
                                                 e(expr::binary_t{token_t::p_less("<"),
                                                                  e(expr::identifier_t{token_t::k_identifier("x")}),
                                                                  e(expr::literal_t{token_t::l_integer("20")})}),
                                             .then_branch = e(expr::literal_t{token_t::l_string("\"medium\"")})})});
}

TEST(ParserTest, ExprIfElseTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        if isValid => "valid"
        else => "invalid"
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                       .condition   = e(expr::identifier_t{token_t::k_identifier("isValid")}),
                       .then_branch = e(expr::literal_t{token_t::l_string("\"valid\"")})}),
                   e(expr::literal_t{token_t::l_string("\"invalid\"")})});
}

TEST(ParserTest, ExprIfElseIfElseTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        if score >= 90 => "A"
        else if score >= 80 => "B"
        else => "C"
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(
                       expr::if_t::conditional_branch_t{
                           .condition   = e(expr::binary_t{token_t::p_greater_equal(">="),
                                                         e(expr::identifier_t{token_t::k_identifier("score")}),
                                                         e(expr::literal_t{token_t::l_integer("90")})}),
                           .then_branch = e(expr::literal_t{token_t::l_string("\"A\"")})},
                       expr::if_t::conditional_branch_t{
                           .condition   = e(expr::binary_t{token_t::p_greater_equal(">="),
                                                         e(expr::identifier_t{token_t::k_identifier("score")}),
                                                         e(expr::literal_t{token_t::l_integer("80")})}),
                           .then_branch = e(expr::literal_t{token_t::l_string("\"B\"")})}),
                   e(expr::literal_t{token_t::l_string("\"C\"")})});
}

TEST(ParserTest, ExprIfBlockTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        if isReady then
            a
        end
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
            .condition   = e(expr::identifier_t{token_t::k_identifier("isReady")}),
            .then_branch = e(expr::block_t{{}, e(expr::identifier_t{token_t::k_identifier("a")})})})});
}

TEST(ParserTest, ExprIfElseMixedBlockTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        if condition then
            a
        else
            b + c
        end
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                       .condition   = e(expr::identifier_t{token_t::k_identifier("condition")}),
                       .then_branch = e(expr::block_t{{}, e(expr::identifier_t{token_t::k_identifier("a")})})}),
                   e(expr::block_t{{},
                                   e(expr::binary_t{token_t::p_plus("+"),
                                                    e(expr::identifier_t{token_t::k_identifier("b")}),
                                                    e(expr::identifier_t{token_t::k_identifier("c")})})})});
}

TEST(ParserTest, ExprIfValueTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        (if x > 0 => x else => -x) * 2
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{
            token_t::p_asterisk("*"),
            e(expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                             .condition   = e(expr::binary_t{token_t::p_greater(">"),
                                                           e(expr::identifier_t{token_t::k_identifier("x")}),
                                                           e(expr::literal_t{token_t::l_integer("0")})}),
                             .then_branch = e(expr::identifier_t{token_t::k_identifier("x")})}),
                         e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("x")})})}),
            e(expr::literal_t{token_t::l_integer("2")})});
}

TEST(ParserTest, ExprWhileTest)
{
    using namespace loxmocha;
    lexer_t lexer{"while a < b => a + 1"};
    parse_expr(lexer).result().visit(test::assert_visitor{},
                                     expr::while_t{e(expr::binary_t{token_t::p_less("<"),
                                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                                    e(expr::identifier_t{token_t::k_identifier("b")})}),
                                                   e(expr::binary_t{token_t::p_plus("+"),
                                                                    e(expr::identifier_t{token_t::k_identifier("a")}),
                                                                    e(expr::literal_t{token_t::l_integer("1")})})});
}

TEST(ParserTest, ExprWhileBlockTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        while count > 0 then
            count - 1
        end
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::while_t{e(expr::binary_t{token_t::p_greater(">"),
                                       e(expr::identifier_t{token_t::k_identifier("count")}),
                                       e(expr::literal_t{token_t::l_integer("0")})}),
                      e(expr::block_t{{},
                                      e(expr::binary_t{token_t::p_minus("-"),
                                                       e(expr::identifier_t{token_t::k_identifier("count")}),
                                                       e(expr::literal_t{token_t::l_integer("1")})})})});
}

TEST(ParserTest, ExprWhileValueTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        (while n > 0 => n - 1) + 10
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_plus("+"),
                       e(expr::while_t{e(expr::binary_t{token_t::p_greater(">"),
                                                        e(expr::identifier_t{token_t::k_identifier("n")}),
                                                        e(expr::literal_t{token_t::l_integer("0")})}),
                                       e(expr::binary_t{token_t::p_minus("-"),
                                                        e(expr::identifier_t{token_t::k_identifier("n")}),
                                                        e(expr::literal_t{token_t::l_integer("1")})})}),
                       e(expr::literal_t{token_t::l_integer("10")})});
}

TEST(ParserTest, ExprBlockTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        begin
            x = x + 1;
            x + 2
        end
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::block_t{
            make_vector<stmt::stmt_t>(stmt::assign_t{e(expr::identifier_t{token_t::k_identifier("x")}),
                                                     e(expr::binary_t{token_t::p_plus("+"),
                                                                      e(expr::identifier_t{token_t::k_identifier("x")}),
                                                                      e(expr::literal_t{token_t::l_integer("1")})})}),
            e(expr::binary_t{token_t::p_plus("+"),
                             e(expr::identifier_t{token_t::k_identifier("x")}),
                             e(expr::literal_t{token_t::l_integer("2")})})});
}

TEST(ParserTest, ExprBlockEmptyTest)
{
    using namespace loxmocha;
    lexer_t lexer{"begin end"};
    parse_expr(lexer).result().visit(test::assert_visitor{}, expr::block_t{{}, e(expr::tuple_t{{}})});
}

TEST(ParserTest, ParserBlockValueTest)
{
    using namespace loxmocha;
    lexer_t lexer{R"(
        (begin a + b end) + 2
    )"};
    parse_expr(lexer).result().visit(
        test::assert_visitor{},
        expr::binary_t{token_t::p_plus("+"),
                       e(expr::block_t{{},
                                       e(expr::binary_t{token_t::p_plus("+"),
                                                        e(expr::identifier_t{token_t::k_identifier("a")}),
                                                        e(expr::identifier_t{token_t::k_identifier("b")})})}),
                       e(expr::literal_t{token_t::l_integer("2")})});
}

TEST(ParserTest, ExprNoRHSLogicalTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a and"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
}

TEST(ParserTest, ExprNoRHSEqualityTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a =="};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
}

TEST(ParserTest, ExprNoRHSComparisonTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a >"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
}

TEST(ParserTest, ExprNoRHSAdditionTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a +"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
}

TEST(ParserTest, ExprNoRHSMultiplicationTest)
{
    using namespace loxmocha;
    lexer_t lexer{"a *"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
}

TEST(ParserTest, ExprFieldNoIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"obj."};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Expected identifier after '.'");
}

TEST(ParserTest, ExprIndexAccessNoExpressionTest)
{
    using namespace loxmocha;
    lexer_t lexer{"arr[2)"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Expected ']' after index expression");
}

TEST(ParserTest, ExprCallNoClosingParenTest)
{
    using namespace loxmocha;
    lexer_t lexer{"foo(2, 3]"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 2);
    EXPECT_EQ(result.diagnostics().front(), "Expected identifier for named argument");
    EXPECT_EQ(result.diagnostics().back(), "Expected ')' after arguments");
}

TEST(ParserTest, ExprGroupingNoClosingParenTest)
{
    using namespace loxmocha;
    lexer_t lexer{"(a + b]"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Expected ')' after expression");
}

TEST(ParserTest, ExprEmptyInputTest)
{
    using namespace loxmocha;
    lexer_t lexer{""};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 1);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected end of input");
}

TEST(ParserTest, ExprInvalidTokenTest)
{
    using namespace loxmocha;
    lexer_t lexer{"+"};
    auto    result = parse_expr(lexer);

    ASSERT_TRUE(!result);
    ASSERT_EQ(result.diagnostics().size(), 2);
    EXPECT_EQ(result.diagnostics().front(), "Unexpected token: +");
    EXPECT_EQ(result.diagnostics().back(), "Unexpected end of input");
}
