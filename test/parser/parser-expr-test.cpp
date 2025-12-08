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
#include <print>
#include <ranges>
#include <string>
#include <vector>

using namespace loxmocha;
using namespace loxmocha::test::helpers;

namespace {
void sunny_day_expr_test(const std::string& source, const expr::expr_t& expected)
{
    lexer_t lexer{source};
    auto    result = parse_expr(lexer);

    EXPECT_TRUE(!!result) << "Parsing has failed with an error";

    for (const auto& diag : result.diagnostics()) {
        std::print("Diagnostic: {}\n", diag);
    }

    result.result().visit(test::assert_visitor{}, expected);
}

void rainy_day_expr_test(const std::string& source, const std::vector<std::string>& expected_diagnostics)
{
    lexer_t lexer{source};
    auto    result = parse_expr(lexer);

    EXPECT_FALSE(!!result) << "Parsing was expected to fail but succeeded";

    EXPECT_EQ(result.diagnostics().size(), expected_diagnostics.size())
        << "Number of diagnostics does not match expected";

    for (const auto& [a_diag, e_diag] : std::views::zip(result.diagnostics(), expected_diagnostics)) {
        EXPECT_EQ(a_diag, e_diag) << "Diagnostic message does not match expected. Expected: " << e_diag
                                  << ", Actual: " << a_diag;
    }
}

} // namespace

TEST(ParserTest, ExprLiteralStringTest)
{
    sunny_day_expr_test(R"("this is a string")", expr::literal_t{token_t::l_string(R"("this is a string")")});
}

TEST(ParserTest, ExprLiteralCharTest) { sunny_day_expr_test(R"('c')", expr::literal_t{token_t::l_char(R"('c')")}); }

TEST(ParserTest, ExprLiteralIntegerTest) { sunny_day_expr_test("12345", expr::literal_t{token_t::l_integer("12345")}); }

TEST(ParserTest, ExprIdentifierTest)
{
    sunny_day_expr_test("my_variable", expr::identifier_t{token_t::k_identifier("my_variable")});
}

TEST(ParserTest, ExprLiteralTrueTest) { sunny_day_expr_test("true", expr::literal_t{token_t::k_true("true")}); }

TEST(ParserTest, ExprLiteralFalseTest) { sunny_day_expr_test("false", expr::literal_t{token_t::k_false("false")}); }

TEST(ParserTest, ExprAndTest)
{
    sunny_day_expr_test("a && b",
                        expr::binary_t{token_t::p_and_and("&&"),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprOrTest)
{
    sunny_day_expr_test("x || y",
                        expr::binary_t{token_t::p_pipe_pipe("||"),
                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                       e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleLogicalTest)
{
    sunny_day_expr_test("a && b || c && d",
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
    sunny_day_expr_test("a == b",
                        expr::binary_t{token_t::p_equal_equal("=="),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprInequalityTest)
{
    sunny_day_expr_test("x != y",
                        expr::binary_t{token_t::p_not_equal("!="),
                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                       e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleEqualityTest)
{
    sunny_day_expr_test(
        "a == b != c == d != e",
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
    sunny_day_expr_test("a == b && c != d",
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
    sunny_day_expr_test("a < b",
                        expr::binary_t{token_t::p_less("<"),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprLessThanOrEqualTest)
{
    sunny_day_expr_test("x <= y",
                        expr::binary_t{token_t::p_less_equal("<="),
                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                       e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprGreaterThanTest)
{
    sunny_day_expr_test("a > b",
                        expr::binary_t{token_t::p_greater(">"),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprGreaterThanOrEqualTest)
{
    sunny_day_expr_test("x >= y",
                        expr::binary_t{token_t::p_greater_equal(">="),
                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                       e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleComparisonTest)
{
    sunny_day_expr_test(
        "a < b <= c > d >= e",
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
    sunny_day_expr_test("a < b == b > c",
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
    sunny_day_expr_test("a + b",
                        expr::binary_t{token_t::p_plus("+"),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprSubtractionTest)
{
    sunny_day_expr_test("x - y",
                        expr::binary_t{token_t::p_minus("-"),
                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                       e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleAdditionTest)
{
    sunny_day_expr_test(
        "a + b - c + d",
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
    sunny_day_expr_test("a + b < c - d",
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
    sunny_day_expr_test("a * b",
                        expr::binary_t{token_t::p_asterisk("*"),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprDivisionTest)
{
    sunny_day_expr_test("x / y",
                        expr::binary_t{token_t::p_slash("/"),
                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                       e(expr::identifier_t{token_t::k_identifier("y")})});
}

TEST(ParserTest, ExprMultipleMultiplicationTest)
{
    sunny_day_expr_test(
        "a * b / c * d",
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
    sunny_day_expr_test("a * b + c / d",
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
    sunny_day_expr_test("-42", expr::unary_t{token_t::p_minus("-"), e(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, ExprLogicalNotTest)
{
    sunny_day_expr_test("!true", expr::unary_t{token_t::p_bang("!"), e(expr::literal_t{token_t::k_true("true")})});
}

TEST(ParserTest, ExprMultipleUnaryTest)
{
    sunny_day_expr_test(
        "-!false",
        expr::unary_t{token_t::p_minus("-"),
                      e(expr::unary_t{token_t::p_bang("!"), e(expr::literal_t{token_t::k_false("false")})})});
}

TEST(ParserTest, ExprUnaryPrecedenceTest)
{
    sunny_day_expr_test(
        "-a * !b",
        expr::binary_t{token_t::p_asterisk("*"),
                       e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                       e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("b")})})});
}

TEST(ParserTest, ExprFieldAccessTest)
{
    sunny_day_expr_test(
        "a.b.c",
        expr::field_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")}),
                      token_t::k_identifier("c")});
}

TEST(ParserTest, ExprIndexAccessTest)
{
    sunny_day_expr_test(
        "matrix[i + 2][j]",
        expr::index_t{e(expr::index_t{e(expr::identifier_t{token_t::k_identifier("matrix")}),
                                      e(expr::binary_t{token_t::p_plus("+"),
                                                       e(expr::identifier_t{token_t::k_identifier("i")}),
                                                       e(expr::literal_t{token_t::l_integer("2")})})}),
                      e(expr::identifier_t{token_t::k_identifier("j")})});
}

TEST(ParserTest, ExprCallNoArgumentsTest)
{
    sunny_day_expr_test("func()", expr::call_t{e(expr::identifier_t{token_t::k_identifier("func")}), {}, {}});
}

TEST(ParserTest, ExprCallPositionalArgumentsTest)
{
    sunny_day_expr_test("add(5, 10)",
                        expr::call_t{e(expr::identifier_t{token_t::k_identifier("add")}),
                                     make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("5")},
                                                               expr::literal_t{token_t::l_integer("10")}),
                                     {}});
}

TEST(ParserTest, ExprCallNamedArgumentsTest)
{
    sunny_day_expr_test(
        "setPosition(x: 100, y: 200)",
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
    sunny_day_expr_test(
        "drawCircle(x + 10, y, radius: 25)",
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
    rainy_day_expr_test("func(a, b: 2, c)", {"Expected ':' after named argument identifier"});
}

TEST(ParserTest, ExprNestedCallsTest)
{
    sunny_day_expr_test(
        "outer(inner(1, 2), another(3))",
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
    sunny_day_expr_test(
        "obj.field[index](arg1, arg2).anotherField",
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
    sunny_day_expr_test(
        "-a.b + c.d[e.f] - -g.h()",
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
    sunny_day_expr_test("(a)", expr::grouping_t{e(expr::identifier_t{token_t::k_identifier("a")})});
}

TEST(ParserTest, ExprIsTest)
{
    sunny_day_expr_test("a is b",
                        expr::is_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                   p(pattern::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprIsPrecedenceTest)
{
    sunny_day_expr_test(
        "!a is b",
        expr::is_t{e(expr::unary_t{token_t::p_bang("!"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                   p(pattern::identifier_t{token_t::k_identifier("b")})});
}

TEST(ParserTest, ExprIsPrecedenceWithGroupingTest)
{
    sunny_day_expr_test(
        "!(a is b)",
        expr::unary_t{token_t::p_bang("!"),
                      e(expr::grouping_t{e(expr::is_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                                      p(pattern::identifier_t{token_t::k_identifier("b")})})})});
}

TEST(ParserTest, ExprIsPrecedenceWithAccessTest)
{
    sunny_day_expr_test(
        "a.b is c",
        expr::is_t{e(expr::field_t{e(expr::identifier_t{token_t::k_identifier("a")}), token_t::k_identifier("b")}),
                   p(pattern::identifier_t{token_t::k_identifier("c")})});
}

TEST(ParserTest, ExprIsPrecedenceWithAndTest)
{
    sunny_day_expr_test("a && b is c",
                        expr::binary_t{token_t::p_and_and("&&"),
                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                       e(expr::is_t{e(expr::identifier_t{token_t::k_identifier("b")}),
                                                    p(pattern::identifier_t{token_t::k_identifier("c")})})});
}

TEST(ParserTest, ExprCastTest)
{
    sunny_day_expr_test("a as MyType",
                        expr::cast_t{e(expr::identifier_t{token_t::k_identifier("a")}),
                                     t(type::identifier_t{token_t::k_identifier("MyType")})});
}

TEST(ParserTest, ExprCastPrecedenceTest)
{
    sunny_day_expr_test(
        "-a as MyType",
        expr::cast_t{e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("a")})}),
                     t(type::identifier_t{token_t::k_identifier("MyType")})});
}

TEST(ParserTest, ExprArrayTest)
{
    sunny_day_expr_test("[1, 2, 3]",
                        expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                expr::literal_t{token_t::l_integer("2")},
                                                                expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprArrayEmptyTest) { sunny_day_expr_test("[]", expr::array_t{{}}); }

TEST(ParserTest, ExprArraySingleElementTest)
{
    sunny_day_expr_test("[42]", expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, ExprArrayTrailingCommaTest)
{
    sunny_day_expr_test("[1, 2, 3, ]",
                        expr::array_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                expr::literal_t{token_t::l_integer("2")},
                                                                expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprArrayPrecedenceTest)
{
    sunny_day_expr_test("[a + b, c * d][i - 1]",
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
    sunny_day_expr_test("{x: 10, y: 20}",
                        expr::record_t{make_vector<expr::record_t::field_t>(
                            expr::record_t::field_t{.name  = token_t::k_identifier("x"),
                                                    .value = expr::literal_t{token_t::l_integer("10")}},
                            expr::record_t::field_t{.name  = token_t::k_identifier("y"),
                                                    .value = expr::literal_t{token_t::l_integer("20")}})});
}

TEST(ParserTest, ExprRecordEmptyTest) { sunny_day_expr_test("{}", expr::record_t{{}}); }

TEST(ParserTest, ExprRecordSingleFieldTest)
{
    sunny_day_expr_test(
        "{value: 42}",
        expr::record_t{make_vector<expr::record_t::field_t>(expr::record_t::field_t{
            .name = token_t::k_identifier("value"), .value = expr::literal_t{token_t::l_integer("42")}})});
}

TEST(ParserTest, ExprRecordTrailingCommaTest)
{
    sunny_day_expr_test("{a: 1, b: 2, }",
                        expr::record_t{make_vector<expr::record_t::field_t>(
                            expr::record_t::field_t{.name  = token_t::k_identifier("a"),
                                                    .value = expr::literal_t{token_t::l_integer("1")}},
                            expr::record_t::field_t{.name  = token_t::k_identifier("b"),
                                                    .value = expr::literal_t{token_t::l_integer("2")}})});
}

TEST(ParserTest, ExprRecordPrecedenceTest)
{
    sunny_day_expr_test(
        "{sum: a + b, product: c * d}.sum",
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
    sunny_day_expr_test("(1, 2, 3)",
                        expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                expr::literal_t{token_t::l_integer("2")},
                                                                expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprTupleEmptyTest) { sunny_day_expr_test("()", expr::tuple_t{{}}); }

TEST(ParserTest, ExprTupleSingleElementTest)
{
    sunny_day_expr_test("(42,)", expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, ExprTupleTrailingCommaTest)
{
    sunny_day_expr_test("(1, 2, 3, )",
                        expr::tuple_t{make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("1")},
                                                                expr::literal_t{token_t::l_integer("2")},
                                                                expr::literal_t{token_t::l_integer("3")})});
}

TEST(ParserTest, ExprTuplePrecedenceTest)
{
    sunny_day_expr_test("(a + b, c * d)[1]",
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
    sunny_day_expr_test("if a && b => c",
                        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                            .condition   = e(expr::binary_t{token_t::p_and_and("&&"),
                                                          e(expr::identifier_t{token_t::k_identifier("a")}),
                                                          e(expr::identifier_t{token_t::k_identifier("b")})}),
                            .then_branch = e(expr::identifier_t{token_t::k_identifier("c")})})});
}

TEST(ParserTest, ExprIfElseIfTest)
{
    sunny_day_expr_test(R"(
        if x < 10 => "small"
        else if x < 20 => "medium"
    )",
                        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(
                            expr::if_t::conditional_branch_t{
                                .condition   = e(expr::binary_t{token_t::p_less("<"),
                                                              e(expr::identifier_t{token_t::k_identifier("x")}),
                                                              e(expr::literal_t{token_t::l_integer("10")})}),
                                .then_branch = e(expr::literal_t{token_t::l_string("\"small\"")})},
                            expr::if_t::conditional_branch_t{
                                .condition   = e(expr::binary_t{token_t::p_less("<"),
                                                              e(expr::identifier_t{token_t::k_identifier("x")}),
                                                              e(expr::literal_t{token_t::l_integer("20")})}),
                                .then_branch = e(expr::literal_t{token_t::l_string("\"medium\"")})})});
}

TEST(ParserTest, ExprIfElseTest)
{
    sunny_day_expr_test(R"(
        if isValid => "valid"
        else => "invalid"
    )",
                        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                                       .condition   = e(expr::identifier_t{token_t::k_identifier("isValid")}),
                                       .then_branch = e(expr::literal_t{token_t::l_string("\"valid\"")})}),
                                   e(expr::literal_t{token_t::l_string("\"invalid\"")})});
}

TEST(ParserTest, ExprIfElseIfElseTest)
{
    sunny_day_expr_test(
        R"(
        if score >= 90 => "A"
        else if score >= 80 => "B"
        else => "C"
    )",
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
    sunny_day_expr_test(R"(
        if isReady then
            a
        end
    )",
                        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                            .condition   = e(expr::identifier_t{token_t::k_identifier("isReady")}),
                            .then_branch = e(expr::block_t{{}, e(expr::identifier_t{token_t::k_identifier("a")})})})});
}

TEST(ParserTest, ExprIfElseMixedBlockTest)
{
    sunny_day_expr_test(
        R"(
        if condition then
            a
        else
            b + c
        end
    )",
        expr::if_t{make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                       .condition   = e(expr::identifier_t{token_t::k_identifier("condition")}),
                       .then_branch = e(expr::block_t{{}, e(expr::identifier_t{token_t::k_identifier("a")})})}),
                   e(expr::binary_t{token_t::p_plus("+"),
                                    e(expr::identifier_t{token_t::k_identifier("b")}),
                                    e(expr::identifier_t{token_t::k_identifier("c")})})});
}

TEST(ParserTest, ExprIfValueTest)
{
    sunny_day_expr_test(
        "(if x > 0 => x else => -x) * 2",
        expr::binary_t{
            token_t::p_asterisk("*"),
            e(expr::grouping_t{e(expr::if_t{
                make_vector<expr::if_t::conditional_branch_t>(expr::if_t::conditional_branch_t{
                    .condition   = e(expr::binary_t{token_t::p_greater(">"),
                                                  e(expr::identifier_t{token_t::k_identifier("x")}),
                                                  e(expr::literal_t{token_t::l_integer("0")})}),
                    .then_branch = e(expr::identifier_t{token_t::k_identifier("x")})}),
                e(expr::unary_t{token_t::p_minus("-"), e(expr::identifier_t{token_t::k_identifier("x")})})})}),
            e(expr::literal_t{token_t::l_integer("2")})});
}

TEST(ParserTest, ExprWhileTest)
{
    sunny_day_expr_test("while a < b => a + 1",
                        expr::while_t{e(expr::binary_t{token_t::p_less("<"),
                                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                                       e(expr::identifier_t{token_t::k_identifier("b")})}),
                                      e(expr::binary_t{token_t::p_plus("+"),
                                                       e(expr::identifier_t{token_t::k_identifier("a")}),
                                                       e(expr::literal_t{token_t::l_integer("1")})})});
}

TEST(ParserTest, ExprWhileBlockTest)
{
    sunny_day_expr_test(
        R"(
        while count > 0 then
            count - 1
        end
    )",
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
    sunny_day_expr_test(
        "(while n > 0 => n - 1) + 10",
        expr::binary_t{
            token_t::p_plus("+"),
            e(expr::grouping_t{e(expr::while_t{e(expr::binary_t{token_t::p_greater(">"),
                                                                e(expr::identifier_t{token_t::k_identifier("n")}),
                                                                e(expr::literal_t{token_t::l_integer("0")})}),
                                               e(expr::binary_t{token_t::p_minus("-"),
                                                                e(expr::identifier_t{token_t::k_identifier("n")}),
                                                                e(expr::literal_t{token_t::l_integer("1")})})})}),
            e(expr::literal_t{token_t::l_integer("10")})});
}

TEST(ParserTest, ExprBlockTest)
{
    sunny_day_expr_test(R"(
        begin
            x = x + 1;
            x + 2
        end
    )",
                        expr::block_t{make_vector<stmt::stmt_t>(stmt::assign_t{
                                          e(expr::identifier_t{token_t::k_identifier("x")}),
                                          e(expr::binary_t{token_t::p_plus("+"),
                                                           e(expr::identifier_t{token_t::k_identifier("x")}),
                                                           e(expr::literal_t{token_t::l_integer("1")})})}),
                                      e(expr::binary_t{token_t::p_plus("+"),
                                                       e(expr::identifier_t{token_t::k_identifier("x")}),
                                                       e(expr::literal_t{token_t::l_integer("2")})})});
}

TEST(ParserTest, ExprBlockEmptyTest) { sunny_day_expr_test("begin end", expr::block_t{{}, e(expr::tuple_t{{}})}); }

TEST(ParserTest, ExprBlockValueTest)
{
    sunny_day_expr_test(
        R"(
        (begin a + b end) + 2
    )",
        expr::binary_t{
            token_t::p_plus("+"),
            e(expr::grouping_t{e(expr::block_t{{},
                                               e(expr::binary_t{token_t::p_plus("+"),
                                                                e(expr::identifier_t{token_t::k_identifier("a")}),
                                                                e(expr::identifier_t{token_t::k_identifier("b")})})})}),
            e(expr::literal_t{token_t::l_integer("2")})});
}

TEST(ParserTest, ExprNoRHSLogicalTest) { rainy_day_expr_test("a &&", {"Unexpected end of input"}); }

TEST(ParserTest, ExprNoRHSEqualityTest) { rainy_day_expr_test("a ==", {"Unexpected end of input"}); }

TEST(ParserTest, ExprNoRHSComparisonTest) { rainy_day_expr_test("a >", {"Unexpected end of input"}); }

TEST(ParserTest, ExprNoRHSAdditionTest) { rainy_day_expr_test("a +", {"Unexpected end of input"}); }

TEST(ParserTest, ExprNoRHSMultiplicationTest) { rainy_day_expr_test("a *", {"Unexpected end of input"}); }

TEST(ParserTest, ExprFieldNoIdentifierTest) { rainy_day_expr_test("obj.", {"Expected identifier after '.'"}); }

TEST(ParserTest, ExprIndexAccessNoExpressionTest)
{
    rainy_day_expr_test("arr[2)", {"Expected ']' after index expression"});
}

TEST(ParserTest, ExprCallNoClosingParenTest)
{
    rainy_day_expr_test("func(2, 3]", {"Expected identifier for named argument", "Expected ')' after arguments"});
}

TEST(ParserTest, ExprGroupingNoClosingParenTest) { rainy_day_expr_test("(a + b]", {"Expected ',' or ')' after expression"}); }

TEST(ParserTest, ExprEmptyInputTest) { rainy_day_expr_test("", {"Unexpected end of input"}); }

TEST(ParserTest, ExprInvalidTokenTest) { rainy_day_expr_test("+", {"Unexpected token: +", "Unexpected end of input"}); }
