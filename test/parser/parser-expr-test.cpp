#include "helpers.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <string>
#include <vector>

using namespace loxmocha;
using namespace loxmocha::ast;
using namespace loxmocha::lexer;
using namespace loxmocha::test::helpers;

TEST_F(ParserTest, ExprLiteralStringTest)
{
    expr_test(R"("this is a string")", expr::literal_t{token_t::l_string(R"("this is a string")")});
}

TEST_F(ParserTest, ExprLiteralCharTest) { expr_test(R"('c')", expr::literal_t{token_t::l_char(R"('c')")}); }

TEST_F(ParserTest, ExprLiteralIntegerTest) { expr_test("12345", expr::literal_t{token_t::l_integer("12345")}); }

TEST_F(ParserTest, ExprIdentifierTest)
{
    expr_test("my_variable", expr::identifier_t{ident_gen.ident("my_variable")});
}

TEST_F(ParserTest, ExprLiteralTrueTest) { expr_test("true", expr::literal_t{token_t::k_true("true")}); }

TEST_F(ParserTest, ExprLiteralFalseTest) { expr_test("false", expr::literal_t{token_t::k_false("false")}); }

TEST_F(ParserTest, ExprAndTest)
{
    expr_test("a && b",
              expr::binary_t{token_t::p_and_and("&&"),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprOrTest)
{
    expr_test("x || y",
              expr::binary_t{token_t::p_pipe_pipe("||"),
                             e(expr::identifier_t{ident_gen.ident("x")}),
                             e(expr::identifier_t{ident_gen.ident("y")})});
}

TEST_F(ParserTest, ExprMultipleLogicalTest)
{
    expr_test("a && b || c && d",
              expr::binary_t{token_t::p_pipe_pipe("||"),
                             e(expr::binary_t{token_t::p_and_and("&&"),
                                              e(expr::identifier_t{ident_gen.ident("a")}),
                                              e(expr::identifier_t{ident_gen.ident("b")})}),
                             e(expr::binary_t{token_t::p_and_and("&&"),
                                              e(expr::identifier_t{ident_gen.ident("c")}),
                                              e(expr::identifier_t{ident_gen.ident("d")})})});
}

TEST_F(ParserTest, ExprEqualityTest)
{
    expr_test("a == b",
              expr::binary_t{token_t::p_equal_equal("=="),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprInequalityTest)
{
    expr_test("x != y",
              expr::binary_t{token_t::p_not_equal("!="),
                             e(expr::identifier_t{ident_gen.ident("x")}),
                             e(expr::identifier_t{ident_gen.ident("y")})});
}

TEST_F(ParserTest, ExprMultipleEqualityTest)
{
    expr_test("a == b != c == d != e",
              expr::binary_t{token_t::p_not_equal("!="),
                             e(expr::binary_t{
                                 token_t::p_equal_equal("=="),
                                 e(expr::binary_t{token_t::p_not_equal("!="),
                                                  e(expr::binary_t{token_t::p_equal_equal("=="),
                                                                   e(expr::identifier_t{ident_gen.ident("a")}),
                                                                   e(expr::identifier_t{ident_gen.ident("b")})}),
                                                  e(expr::identifier_t{ident_gen.ident("c")})}),
                                 e(expr::identifier_t{ident_gen.ident("d")})}),
                             e(expr::identifier_t{ident_gen.ident("e")})});
}

TEST_F(ParserTest, ExprEqualityPrecedenceTest)
{
    expr_test("a == b && c != d",
              expr::binary_t{token_t::p_and_and("&&"),
                             e(expr::binary_t{token_t::p_equal_equal("=="),
                                              e(expr::identifier_t{ident_gen.ident("a")}),
                                              e(expr::identifier_t{ident_gen.ident("b")})}),
                             e(expr::binary_t{token_t::p_not_equal("!="),
                                              e(expr::identifier_t{ident_gen.ident("c")}),
                                              e(expr::identifier_t{ident_gen.ident("d")})})});
}

TEST_F(ParserTest, ExprLessThanTest)
{
    expr_test("a < b",
              expr::binary_t{token_t::p_less("<"),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprLessThanOrEqualTest)
{
    expr_test("x <= y",
              expr::binary_t{token_t::p_less_equal("<="),
                             e(expr::identifier_t{ident_gen.ident("x")}),
                             e(expr::identifier_t{ident_gen.ident("y")})});
}

TEST_F(ParserTest, ExprGreaterThanTest)
{
    expr_test("a > b",
              expr::binary_t{token_t::p_greater(">"),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprGreaterThanOrEqualTest)
{
    expr_test("x >= y",
              expr::binary_t{token_t::p_greater_equal(">="),
                             e(expr::identifier_t{ident_gen.ident("x")}),
                             e(expr::identifier_t{ident_gen.ident("y")})});
}

TEST_F(ParserTest, ExprMultipleComparisonTest)
{
    expr_test("a < b <= c > d >= e",
              expr::binary_t{token_t::p_greater_equal(">="),
                             e(expr::binary_t{
                                 token_t::p_greater(">"),
                                 e(expr::binary_t{token_t::p_less_equal("<="),
                                                  e(expr::binary_t{token_t::p_less("<"),
                                                                   e(expr::identifier_t{ident_gen.ident("a")}),
                                                                   e(expr::identifier_t{ident_gen.ident("b")})}),
                                                  e(expr::identifier_t{ident_gen.ident("c")})}),
                                 e(expr::identifier_t{ident_gen.ident("d")})}),
                             e(expr::identifier_t{ident_gen.ident("e")})});
}

TEST_F(ParserTest, ExprComparisonPrecedenceTest)
{
    expr_test("a < b == b > c",
              expr::binary_t{token_t::p_equal_equal("=="),
                             e(expr::binary_t{token_t::p_less("<"),
                                              e(expr::identifier_t{ident_gen.ident("a")}),
                                              e(expr::identifier_t{ident_gen.ident("b")})}),
                             e(expr::binary_t{token_t::p_greater(">"),
                                              e(expr::identifier_t{ident_gen.ident("b")}),
                                              e(expr::identifier_t{ident_gen.ident("c")})})});
}

TEST_F(ParserTest, ExprAdditionTest)
{
    expr_test("a + b",
              expr::binary_t{token_t::p_plus("+"),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprSubtractionTest)
{
    expr_test("x - y",
              expr::binary_t{token_t::p_minus("-"),
                             e(expr::identifier_t{ident_gen.ident("x")}),
                             e(expr::identifier_t{ident_gen.ident("y")})});
}

TEST_F(ParserTest, ExprMultipleAdditionTest)
{
    expr_test("a + b - c + d",
              expr::binary_t{token_t::p_plus("+"),
                             e(expr::binary_t{token_t::p_minus("-"),
                                              e(expr::binary_t{token_t::p_plus("+"),
                                                               e(expr::identifier_t{ident_gen.ident("a")}),
                                                               e(expr::identifier_t{ident_gen.ident("b")})}),
                                              e(expr::identifier_t{ident_gen.ident("c")})}),
                             e(expr::identifier_t{ident_gen.ident("d")})});
}

TEST_F(ParserTest, ExprAdditionPrecedenceTest)
{
    expr_test("a + b < c - d",
              expr::binary_t{token_t::p_less("<"),
                             e(expr::binary_t{token_t::p_plus("+"),
                                              e(expr::identifier_t{ident_gen.ident("a")}),
                                              e(expr::identifier_t{ident_gen.ident("b")})}),
                             e(expr::binary_t{token_t::p_minus("-"),
                                              e(expr::identifier_t{ident_gen.ident("c")}),
                                              e(expr::identifier_t{ident_gen.ident("d")})})});
}

TEST_F(ParserTest, ExprMultiplicationTest)
{
    expr_test("a * b",
              expr::binary_t{token_t::p_asterisk("*"),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprDivisionTest)
{
    expr_test("x / y",
              expr::binary_t{token_t::p_slash("/"),
                             e(expr::identifier_t{ident_gen.ident("x")}),
                             e(expr::identifier_t{ident_gen.ident("y")})});
}

TEST_F(ParserTest, ExprMultipleMultiplicationTest)
{
    expr_test("a * b / c * d",
              expr::binary_t{token_t::p_asterisk("*"),
                             e(expr::binary_t{token_t::p_slash("/"),
                                              e(expr::binary_t{token_t::p_asterisk("*"),
                                                               e(expr::identifier_t{ident_gen.ident("a")}),
                                                               e(expr::identifier_t{ident_gen.ident("b")})}),
                                              e(expr::identifier_t{ident_gen.ident("c")})}),
                             e(expr::identifier_t{ident_gen.ident("d")})});
}

TEST_F(ParserTest, ExprMultiplicationPrecedenceTest)
{
    expr_test("a * b + c / d",
              expr::binary_t{token_t::p_plus("+"),
                             e(expr::binary_t{token_t::p_asterisk("*"),
                                              e(expr::identifier_t{ident_gen.ident("a")}),
                                              e(expr::identifier_t{ident_gen.ident("b")})}),
                             e(expr::binary_t{token_t::p_slash("/"),
                                              e(expr::identifier_t{ident_gen.ident("c")}),
                                              e(expr::identifier_t{ident_gen.ident("d")})})});
}

TEST_F(ParserTest, ExprNegationTest)
{
    expr_test("-42", expr::unary_t{token_t::p_minus("-"), e(expr::literal_t{token_t::l_integer("42")})});
}

TEST_F(ParserTest, ExprLogicalNotTest)
{
    expr_test("!true", expr::unary_t{token_t::p_bang("!"), e(expr::literal_t{token_t::k_true("true")})});
}

TEST_F(ParserTest, ExprMultipleUnaryTest)
{
    expr_test("-!false",
              expr::unary_t{token_t::p_minus("-"),
                            e(expr::unary_t{token_t::p_bang("!"), e(expr::literal_t{token_t::k_false("false")})})});
}

TEST_F(ParserTest, ExprUnaryPrecedenceTest)
{
    expr_test(
        "-a * !b",
        expr::binary_t{token_t::p_asterisk("*"),
                       e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{ident_gen.ident("a")})}),
                       e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{ident_gen.ident("b")})})});
}

TEST_F(ParserTest, ExprFieldAccessTest)
{
    expr_test(
        "a.b.c",
        expr::field_t{e(expr::field_t{e(expr::identifier_t{ident_gen.ident("a")}), ident_gen.ident("b")}),
                      ident_gen.ident("c")});
}

TEST_F(ParserTest, ExprIndexAccessTest)
{
    expr_test("matrix[i + 2][j]",
              expr::index_t{e(expr::index_t{e(expr::identifier_t{ident_gen.ident("matrix")}),
                                            e(expr::binary_t{token_t::p_plus("+"),
                                                             e(expr::identifier_t{ident_gen.ident("i")}),
                                                             e(expr::literal_t{token_t::l_integer("2")})})}),
                            e(expr::identifier_t{ident_gen.ident("j")})});
}

TEST_F(ParserTest, ExprCallNoArgumentsTest)
{
    expr_test("func()", expr::call_t{e(expr::identifier_t{ident_gen.ident("func")}), {}, {}});
}

TEST_F(ParserTest, ExprCallPositionalArgumentsTest)
{
    expr_test("add(5, 10)",
              expr::call_t{e(expr::identifier_t{ident_gen.ident("add")}),
                           make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("5")},
                                                     expr::literal_t{token_t::l_integer("10")}),
                           {}});
}

TEST_F(ParserTest, ExprCallNamedArgumentsTest)
{
    expr_test(
        "setPosition(x: 100, y: 200)",
        expr::call_t{
            e(expr::identifier_t{ident_gen.ident("setPosition")}),
            {},
            make_vector2<expr::call_t::named_arg_t>(
                expr::call_t::named_arg_t{.name  = ident_gen.ident("x"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("100")}}},
                expr::call_t::named_arg_t{.name  = ident_gen.ident("y"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("200")}}})});
}

TEST_F(ParserTest, ExprCallMixedArgumentTest)
{
    expr_test("drawCircle(x + 10, y, radius: 25)",
              expr::call_t{e(expr::identifier_t{ident_gen.ident("drawCircle")}),
                           make_vector<expr::expr_t>(expr::binary_t{token_t::p_plus("+"),
                                                                    e(expr::identifier_t{ident_gen.ident("x")}),
                                                                    e(expr::literal_t{token_t::l_integer("10")})},
                                                     expr::identifier_t{ident_gen.ident("y")}),
                           make_vector2<expr::call_t::named_arg_t>(expr::call_t::named_arg_t{
                               .name  = ident_gen.ident("radius"),
                               .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("25")}}})});
}

TEST_F(ParserTest, ExprPositionalAfterNamedArgsTest)
{
    rainy_day_expr_test("func(a, b: 2, c)", {"Expected ':' after named argument identifier"});
}

TEST_F(ParserTest, ExprNestedCallsTest)
{
    expr_test("outer(inner(1, 2), another(3))",
              expr::call_t{e(expr::identifier_t{ident_gen.ident("outer")}),
                           make_vector<expr::expr_t>(
                               expr::call_t{e(expr::identifier_t{ident_gen.ident("inner")}),
                                            make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                      expr::literal_t{token_t::l_integer("2")}),
                                            {}},
                               expr::call_t{e(expr::identifier_t{ident_gen.ident("another")}),
                                            make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("3")}),
                                            {}}),
                           {}});
}

TEST_F(ParserTest, ExprMixedAccessTest)
{
    expr_test("obj.field[index](arg1, arg2).anotherField",
              expr::field_t{
                  e(expr::call_t{e(expr::index_t{e(expr::field_t{e(expr::identifier_t{ident_gen.ident("obj")}),
                                                                 ident_gen.ident("field")}),
                                                 e(expr::identifier_t{ident_gen.ident("index")})}),
                                 make_vector<expr::expr_t>(expr::identifier_t{ident_gen.ident("arg1")},
                                                           expr::identifier_t{ident_gen.ident("arg2")}),
                                 {}}),
                  ident_gen.ident("anotherField")});
}

TEST_F(ParserTest, ExprAccessPrecedenceTest)
{
    expr_test(
        "-a.b + c.d[e.f] - -g.h()",
        expr::binary_t{
            token_t::p_minus("-"),
            e(expr::binary_t{
                token_t::p_plus("+"),
                e(expr::unary_t{
                    token_t::p_minus("-"),
                    e(expr::field_t{e(expr::identifier_t{ident_gen.ident("a")}), ident_gen.ident("b")})}),
                e(expr::index_t{
                    e(expr::field_t{e(expr::identifier_t{ident_gen.ident("c")}), ident_gen.ident("d")}),
                    e(expr::field_t{e(expr::identifier_t{ident_gen.ident("e")}), ident_gen.ident("f")})})}),
            e(expr::unary_t{token_t::p_minus("-"),
                            e(expr::call_t{e(expr::field_t{e(expr::identifier_t{ident_gen.ident("g")}),
                                                           ident_gen.ident("h")}),
                                           {},
                                           {}})})});
}

TEST_F(ParserTest, ExprGroupingTest)
{
    expr_test("(a)", expr::grouping_t{e(expr::identifier_t{ident_gen.ident("a")})});
}

TEST_F(ParserTest, ExprIsTest)
{
    expr_test("a is b",
              expr::is_t{e(expr::identifier_t{ident_gen.ident("a")}),
                         p(pattern::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprIsPrecedenceTest)
{
    expr_test("!a is b",
              expr::is_t{e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{ident_gen.ident("a")})}),
                         p(pattern::identifier_t{ident_gen.ident("b")})});
}

TEST_F(ParserTest, ExprIsPrecedenceWithGroupingTest)
{
    expr_test("!(a is b)",
              expr::unary_t{token_t::p_bang("!"),
                            e(expr::grouping_t{e(expr::is_t{e(expr::identifier_t{ident_gen.ident("a")}),
                                                            p(pattern::identifier_t{ident_gen.ident("b")})})})});
}

TEST_F(ParserTest, ExprIsPrecedenceWithAccessTest)
{
    expr_test(
        "a.b is c",
        expr::is_t{e(expr::field_t{e(expr::identifier_t{ident_gen.ident("a")}), ident_gen.ident("b")}),
                   p(pattern::identifier_t{ident_gen.ident("c")})});
}

TEST_F(ParserTest, ExprIsPrecedenceWithAndTest)
{
    expr_test("a && b is c",
              expr::binary_t{token_t::p_and_and("&&"),
                             e(expr::identifier_t{ident_gen.ident("a")}),
                             e(expr::is_t{e(expr::identifier_t{ident_gen.ident("b")}),
                                          p(pattern::identifier_t{ident_gen.ident("c")})})});
}

TEST_F(ParserTest, ExprCastTest)
{
    expr_test("a as MyType",
              expr::cast_t{e(expr::identifier_t{ident_gen.ident("a")}),
                           t(type::identifier_t{ident_gen.ident("MyType")})});
}

TEST_F(ParserTest, ExprCastPrecedenceTest)
{
    expr_test("-a as MyType",
              expr::cast_t{e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{ident_gen.ident("a")})}),
                           t(type::identifier_t{ident_gen.ident("MyType")})});
}

TEST_F(ParserTest, ExprArrayTest)
{
    expr_test("[1, 2, 3]",
              expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                      expr::literal_t{token_t::l_integer("2")},
                                                      expr::literal_t{token_t::l_integer("3")})});
}

TEST_F(ParserTest, ExprArrayEmptyTest) { expr_test("[]", expr::array_t{{}}); }

TEST_F(ParserTest, ExprArraySingleElementTest)
{
    expr_test("[42]", expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("42")})});
}

TEST_F(ParserTest, ExprArrayTrailingCommaTest)
{
    expr_test("[1, 2, 3, ]",
              expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                      expr::literal_t{token_t::l_integer("2")},
                                                      expr::literal_t{token_t::l_integer("3")})});
}

TEST_F(ParserTest, ExprArrayPrecedenceTest)
{
    expr_test("[a + b, c * d][i - 1]",
              expr::index_t{e(expr::array_t{make_vector<expr::expr_t>(
                                expr::binary_t{token_t::p_plus("+"),
                                               e(expr::identifier_t{ident_gen.ident("a")}),
                                               e(expr::identifier_t{ident_gen.ident("b")})},
                                expr::binary_t{token_t::p_asterisk("*"),
                                               e(expr::identifier_t{ident_gen.ident("c")}),
                                               e(expr::identifier_t{ident_gen.ident("d")})})}),
                            e(expr::binary_t{token_t::p_minus("-"),
                                             e(expr::identifier_t{ident_gen.ident("i")}),
                                             e(expr::literal_t{token_t::l_integer("1")})})});
}

TEST_F(ParserTest, ExprRecordTest)
{
    expr_test("{x: 10, y: 20}",
              expr::record_t{make_vector2<expr::record_t::field_t>(
                  expr::record_t::field_t{.name  = ident_gen.ident("x"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("10")}}},
                  expr::record_t::field_t{.name  = ident_gen.ident("y"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("20")}}})});
}

TEST_F(ParserTest, ExprRecordEmptyTest) { expr_test("{}", expr::record_t{{}}); }

TEST_F(ParserTest, ExprRecordSingleFieldTest)
{
    expr_test("{value: 42}",
              expr::record_t{make_vector2<expr::record_t::field_t>(
                  expr::record_t::field_t{.name  = ident_gen.ident("value"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("42")}}})});
}

TEST_F(ParserTest, ExprRecordTrailingCommaTest)
{
    expr_test("{a: 1, b: 2, }",
              expr::record_t{make_vector2<expr::record_t::field_t>(
                  expr::record_t::field_t{.name  = ident_gen.ident("a"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("1")}}},
                  expr::record_t::field_t{.name  = ident_gen.ident("b"),
                                          .value = expr::expr_t{"", expr::literal_t{token_t::l_integer("2")}}})});
}

TEST_F(ParserTest, ExprRecordPrecedenceTest)
{
    expr_test(
        "{sum: a + b, product: c * d}.sum",
        expr::field_t{
            e(expr::record_t{make_vector2<expr::record_t::field_t>(
                expr::record_t::field_t{
                    .name  = ident_gen.ident("sum"),
                    .value = expr::expr_t{"",
                                          expr::binary_t{token_t::p_plus("+"),
                                                         e(expr::identifier_t{ident_gen.ident("a")}),
                                                         e(expr::identifier_t{ident_gen.ident("b")})}}},
                expr::record_t::field_t{
                    .name  = ident_gen.ident("product"),
                    .value = expr::expr_t{"",
                                          expr::binary_t{token_t::p_asterisk("*"),
                                                         e(expr::identifier_t{ident_gen.ident("c")}),
                                                         e(expr::identifier_t{ident_gen.ident("d")})}}})}),
            ident_gen.ident("sum")});
}

TEST_F(ParserTest, ExprTupleTest)
{
    expr_test("(1, 2, 3)",
              expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                      expr::literal_t{token_t::l_integer("2")},
                                                      expr::literal_t{token_t::l_integer("3")})});
}

TEST_F(ParserTest, ExprTupleEmptyTest) { expr_test("()", expr::tuple_t{{}}); }

TEST_F(ParserTest, ExprTupleSingleElementTest)
{
    expr_test("(42,)", expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("42")})});
}

TEST_F(ParserTest, ExprTupleTrailingCommaTest)
{
    expr_test("(1, 2, 3, )",
              expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                      expr::literal_t{token_t::l_integer("2")},
                                                      expr::literal_t{token_t::l_integer("3")})});
}

TEST_F(ParserTest, ExprTuplePrecedenceTest)
{
    expr_test("(a + b, c * d)[1]",
              expr::index_t{e(expr::tuple_t{make_vector<expr::expr_t>(
                                expr::binary_t{token_t::p_plus("+"),
                                               e(expr::identifier_t{ident_gen.ident("a")}),
                                               e(expr::identifier_t{ident_gen.ident("b")})},
                                expr::binary_t{token_t::p_asterisk("*"),
                                               e(expr::identifier_t{ident_gen.ident("c")}),
                                               e(expr::identifier_t{ident_gen.ident("d")})})}),
                            e(expr::literal_t{token_t::l_integer("1")})});
}

TEST_F(ParserTest, ExprIfTest)
{
    expr_test("if a && b => c",
              expr::if_t{make_vector2<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                  .condition   = expr::expr_t{"",
                                            expr::binary_t{token_t::p_and_and("&&"),
                                                           e(expr::identifier_t{ident_gen.ident("a")}),
                                                           e(expr::identifier_t{ident_gen.ident("b")})}},
                  .then_branch = expr::expr_t{"", expr::identifier_t{ident_gen.ident("c")}}})});
}

TEST_F(ParserTest, ExprIfElseIfTest)
{
    expr_test(R"(
        if x < 10 => "small"
        else if x < 20 => "medium"
    )",
              expr::if_t{make_vector2<expr::if_t::conditional_branch_t>(
                  expr::if_t::conditional_branch_t{
                      .condition   = expr::expr_t{"",
                                                expr::binary_t{token_t::p_less("<"),
                                                               e(expr::identifier_t{ident_gen.ident("x")}),
                                                               e(expr::literal_t{token_t::l_integer("10")})}},
                      .then_branch = expr::expr_t{"", expr::literal_t{token_t::l_string("\"small\"")}}},
                  expr::if_t::conditional_branch_t{
                      .condition   = expr::expr_t{"",
                                                expr::binary_t{token_t::p_less("<"),
                                                               e(expr::identifier_t{ident_gen.ident("x")}),
                                                               e(expr::literal_t{token_t::l_integer("20")})}},
                      .then_branch = expr::expr_t{"", expr::literal_t{token_t::l_string("\"medium\"")}}})});
}

TEST_F(ParserTest, ExprIfElseTest)
{
    expr_test(R"(
        if isValid => "valid"
        else => "invalid"
    )",
              expr::if_t{make_vector2<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                             .condition   = expr::expr_t{"", expr::identifier_t{ident_gen.ident("isValid")}},
                             .then_branch = expr::expr_t{"", expr::literal_t{token_t::l_string("\"valid\"")}}}),
                         e(expr::literal_t{token_t::l_string("\"invalid\"")})});
}

TEST_F(ParserTest, ExprIfElseIfElseTest)
{
    expr_test(
        R"(
        if score >= 90 => "A"
        else if score >= 80 => "B"
        else => "C"
    )",
        expr::if_t{
            make_vector2<expr::if_t::conditional_branch_t>(
                expr::if_t::conditional_branch_t{
                    .condition   = expr::expr_t{"",
                                              expr::binary_t{token_t::p_greater_equal(">="),
                                                             e(expr::identifier_t{ident_gen.ident("score")}),
                                                             e(expr::literal_t{token_t::l_integer("90")})}},
                    .then_branch = expr::expr_t{"", expr::literal_t{token_t::l_string("\"A\"")}}},
                expr::if_t::conditional_branch_t{
                    .condition   = expr::expr_t{"",
                                              expr::binary_t{token_t::p_greater_equal(">="),
                                                             e(expr::identifier_t{ident_gen.ident("score")}),
                                                             e(expr::literal_t{token_t::l_integer("80")})}},
                    .then_branch = expr::expr_t{"", expr::literal_t{token_t::l_string("\"B\"")}}}),
            e(expr::literal_t{token_t::l_string("\"C\"")})});
}

TEST_F(ParserTest, ExprIfBlockTest)
{
    expr_test(
        R"(
        if isReady then
            a
        end
    )",
        expr::if_t{make_vector2<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
            .condition   = expr::expr_t{"", expr::identifier_t{ident_gen.ident("isReady")}},
            .then_branch = expr::expr_t{"", expr::block_t{{}, e(expr::identifier_t{ident_gen.ident("a")})}}})});
}

TEST_F(ParserTest, ExprIfElseMixedBlockTest)
{
    expr_test(
        R"(
        if condition then
            a
        else
            b + c
        end
    )",
        expr::if_t{
            make_vector2<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                .condition   = expr::expr_t{"", expr::identifier_t{ident_gen.ident("condition")}},
                .then_branch = expr::expr_t{"", expr::block_t{{}, e(expr::identifier_t{ident_gen.ident("a")})}}}),
            e(expr::block_t{{},
                            e(expr::binary_t{token_t::p_plus("+"),
                                             e(expr::identifier_t{ident_gen.ident("b")}),
                                             e(expr::identifier_t{ident_gen.ident("c")})})})});
}

TEST_F(ParserTest, ExprIfValueTest)
{
    expr_test("(if x > 0 => x else => -x) * 2",
              expr::binary_t{
                  token_t::p_asterisk("*"),
                  e(expr::grouping_t{e(expr::if_t{
                      make_vector2<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                          .condition   = expr::expr_t{"",
                                                    expr::binary_t{token_t::p_greater(">"),
                                                                   e(expr::identifier_t{ident_gen.ident("x")}),
                                                                   e(expr::literal_t{token_t::l_integer("0")})}},
                          .then_branch = expr::expr_t{"", expr::identifier_t{ident_gen.ident("x")}}}),
                      e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{ident_gen.ident("x")})})})}),
                  e(expr::literal_t{token_t::l_integer("2")})});
}

TEST_F(ParserTest, ExprWhileTest)
{
    expr_test("while a < b => a + 1",
              expr::while_t{e(expr::binary_t{token_t::p_less("<"),
                                             e(expr::identifier_t{ident_gen.ident("a")}),
                                             e(expr::identifier_t{ident_gen.ident("b")})}),
                            e(expr::binary_t{token_t::p_plus("+"),
                                             e(expr::identifier_t{ident_gen.ident("a")}),
                                             e(expr::literal_t{token_t::l_integer("1")})})});
}

TEST_F(ParserTest, ExprWhileBlockTest)
{
    expr_test(
        R"(
        while count > 0 then
            count - 1
        end
    )",
        expr::while_t{e(expr::binary_t{token_t::p_greater(">"),
                                       e(expr::identifier_t{ident_gen.ident("count")}),
                                       e(expr::literal_t{token_t::l_integer("0")})}),
                      e(expr::block_t{{},
                                      e(expr::binary_t{token_t::p_minus("-"),
                                                       e(expr::identifier_t{ident_gen.ident("count")}),
                                                       e(expr::literal_t{token_t::l_integer("1")})})})});
}

TEST_F(ParserTest, ExprWhileValueTest)
{
    expr_test("(while n > 0 => n - 1) + 10",
              expr::binary_t{
                  token_t::p_plus("+"),
                  e(expr::grouping_t{e(expr::while_t{e(expr::binary_t{token_t::p_greater(">"),
                                                                      e(expr::identifier_t{ident_gen.ident("n")}),
                                                                      e(expr::literal_t{token_t::l_integer("0")})}),
                                                     e(expr::binary_t{token_t::p_minus("-"),
                                                                      e(expr::identifier_t{ident_gen.ident("n")}),
                                                                      e(expr::literal_t{token_t::l_integer("1")})})})}),
                  e(expr::literal_t{token_t::l_integer("10")})});
}

TEST_F(ParserTest, ExprBlockTest)
{
    expr_test(R"(
        begin
            x = x + 1;
            x + 2
        end
    )",
              expr::block_t{make_vector<stmt::stmt_t>(
                                stmt::assign_t{e(expr::identifier_t{ident_gen.ident("x")}),
                                               e(expr::binary_t{token_t::p_plus("+"),
                                                                e(expr::identifier_t{ident_gen.ident("x")}),
                                                                e(expr::literal_t{token_t::l_integer("1")})})}),
                            e(expr::binary_t{token_t::p_plus("+"),
                                             e(expr::identifier_t{ident_gen.ident("x")}),
                                             e(expr::literal_t{token_t::l_integer("2")})})});
}

TEST_F(ParserTest, ExprBlockEmptyTest) { expr_test("begin end", expr::block_t{{}, e(expr::tuple_t{{}})}); }

TEST_F(ParserTest, ExprBlockValueTest)
{
    expr_test(
        R"(
        (begin a + b end) + 2
    )",
        expr::binary_t{
            token_t::p_plus("+"),
            e(expr::grouping_t{e(expr::block_t{{},
                                               e(expr::binary_t{token_t::p_plus("+"),
                                                                e(expr::identifier_t{ident_gen.ident("a")}),
                                                                e(expr::identifier_t{ident_gen.ident("b")})})})}),
            e(expr::literal_t{token_t::l_integer("2")})});
}

TEST_F(ParserTest, ExprBlockNoReturnTest)
{
    expr_test(
        R"(
        begin
            x = x + 1;
            y = y + 2;
        end
    )",
        expr::block_t{
            make_vector<stmt::stmt_t>(stmt::assign_t{e(expr::identifier_t{ident_gen.ident("x")}),
                                                     e(expr::binary_t{token_t::p_plus("+"),
                                                                      e(expr::identifier_t{ident_gen.ident("x")}),
                                                                      e(expr::literal_t{token_t::l_integer("1")})})},
                                      stmt::assign_t{e(expr::identifier_t{ident_gen.ident("y")}),
                                                     e(expr::binary_t{token_t::p_plus("+"),
                                                                      e(expr::identifier_t{ident_gen.ident("y")}),
                                                                      e(expr::literal_t{token_t::l_integer("2")})})}),
            e(expr::tuple_t{{}})});
}

TEST_F(ParserTest, ExprNoRHSLogicalTest) { rainy_day_expr_test("a &&", {"Unexpected end of input"}); }

TEST_F(ParserTest, ExprNoRHSEqualityTest) { rainy_day_expr_test("a ==", {"Unexpected end of input"}); }

TEST_F(ParserTest, ExprNoRHSComparisonTest) { rainy_day_expr_test("a >", {"Unexpected end of input"}); }

TEST_F(ParserTest, ExprNoRHSAdditionTest) { rainy_day_expr_test("a +", {"Unexpected end of input"}); }

TEST_F(ParserTest, ExprNoRHSMultiplicationTest) { rainy_day_expr_test("a *", {"Unexpected end of input"}); }

TEST_F(ParserTest, ExprFieldNoIdentifierTest) { rainy_day_expr_test("obj.", {"Expected identifier after '.'"}); }

TEST_F(ParserTest, ExprIndexAccessNoExpressionTest)
{
    rainy_day_expr_test("arr[2)", {"Expected ']' after index expression"});
}

TEST_F(ParserTest, ExprCallNoClosingParenTest)
{
    rainy_day_expr_test("func(2, 3]", {"Expected identifier for named argument", "Expected ')' after arguments"});
}

TEST_F(ParserTest, ExprGroupingNoClosingParenTest)
{
    rainy_day_expr_test("(a + b]", {"Expected ',' or ')' after expression"});
}

TEST_F(ParserTest, ExprEmptyInputTest) { rainy_day_expr_test("", {"Unexpected end of input"}); }

TEST_F(ParserTest, ExprInvalidTokenTest) { rainy_day_expr_test("+", {"Unexpected token: +", "Unexpected end of input"}); }
