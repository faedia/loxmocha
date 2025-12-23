#include "helpers.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <vector>

using namespace loxmocha;
using namespace loxmocha::ast;
using namespace loxmocha::lexer;
using namespace loxmocha::test::helpers;

TEST_F(ParserTest, TypeIdentifierTest) { type_test("my_type", type::identifier_t{ident_gen.ident("my_type")}); }

TEST_F(ParserTest, TypeArrayTest)
{
    type_test("MyType[5]",
              type::array_t{t(type::identifier_t{ident_gen.ident("MyType")}),
                            e(expr::literal_t{token_t::l_integer("5")})});
}

TEST_F(ParserTest, TypeMultiDimensionalArrayTest)
{
    type_test("MyType[3][const_func()]",
              type::array_t{t(type::array_t{t(type::identifier_t{ident_gen.ident("MyType")}),
                                            e(expr::literal_t{token_t::l_integer("3")})}),
                            e(expr::call_t{e(expr::identifier_t{ident_gen.ident("const_func")}), {}, {}})});
}

TEST_F(ParserTest, TypeTupleTest)
{
    type_test("(int, string, arr[5])",
              type::tuple_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")},
                                                      type::identifier_t{ident_gen.ident("string")},
                                                      type::array_t{t(type::identifier_t{ident_gen.ident("arr")}),
                                                                    e(expr::literal_t{token_t::l_integer("5")})})});
}

TEST_F(ParserTest, TypeEmptyTupleTest) { type_test("()", type::tuple_t{{}}); }

TEST_F(ParserTest, TypeTupleSingleElementTest)
{
    type_test("(int,)", type::tuple_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")})});
}

TEST_F(ParserTest, TypeTupleTrailingCommaTest)
{
    type_test("(int, arr[10], string,)",
              type::tuple_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")},
                                                      type::array_t{t(type::identifier_t{ident_gen.ident("arr")}),
                                                                    e(expr::literal_t{token_t::l_integer("10")})},
                                                      type::identifier_t{ident_gen.ident("string")})});
}

TEST_F(ParserTest, TypeRecordTest)
{
    type_test(
        R"(
    rec
        a: int,
        b: string
    end
    )",
        type::record_t{make_vector2<type::record_t::field_t>(
            type::record_t::field_t{.name = ident_gen.ident("a"),
                                    .type = type::type_t{"", type::identifier_t{ident_gen.ident("int")}}},
            type::record_t::field_t{.name = ident_gen.ident("b"),
                                    .type = type::type_t{"", type::identifier_t{ident_gen.ident("string")}}})});
}

TEST_F(ParserTest, TypeEmptyRecordTest) { type_test("rec end", type::record_t{{}}); }

TEST_F(ParserTest, TypeTaggedTest)
{
    type_test(
        R"(
    choice
        FirstTag: int,
        SecondTag: rec
            name: string,
            value: int
        end
    end
    )",
        type::tagged_t{make_vector2<type::tagged_t::tag_t>(
            type::tagged_t::tag_t{.name = ident_gen.ident("FirstTag"),
                                  .type = type::type_t{"", type::identifier_t{ident_gen.ident("int")}}},
            type::tagged_t::tag_t{
                .name = ident_gen.ident("SecondTag"),
                .type =
                    type::type_t{"",
                                 type::record_t{make_vector2<type::record_t::field_t>(
                                     type::record_t::field_t{
                                         .name = ident_gen.ident("name"),
                                         .type = type::type_t{"", type::identifier_t{ident_gen.ident("string")}}},
                                     type::record_t::field_t{
                                         .name = ident_gen.ident("value"),
                                         .type = type::type_t{"", type::identifier_t{ident_gen.ident("int")}}})}

                    }}

            )});
}

TEST_F(ParserTest, TypeSingleTaggedTest)
{
    type_test("choice SingleTag: () end",
              type::tagged_t{make_vector2<type::tagged_t::tag_t>(type::tagged_t::tag_t{
                  .name = ident_gen.ident("SingleTag"), .type = type::type_t{"", type::tuple_t{{}}}})});
}

TEST_F(ParserTest, TypeReferenceTest)
{
    type_test("let int", type::reference_t{t(type::identifier_t{ident_gen.ident("int")})});
}

TEST_F(ParserTest, TypeMutableReferenceTest)
{
    type_test("var string", type::mutable_t{t(type::identifier_t{ident_gen.ident("string")})});
}

TEST_F(ParserTest, TypeFunctionTest)
{
    type_test("fun(int, (string, float)): bool",
              type::function_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")},
                                                         type::tuple_t{make_vector<type::type_t>(
                                                             type::identifier_t{ident_gen.ident("string")},
                                                             type::identifier_t{ident_gen.ident("float")})}),
                               t(type::identifier_t{ident_gen.ident("bool")})});
}

TEST_F(ParserTest, TypeFunctionNoParametersTest)
{
    type_test("fun(): int",
              type::function_t{std::vector<type::type_t>{}, t(type::identifier_t{ident_gen.ident("int")})});
}

TEST_F(ParserTest, TypeFunctionNoReturnTypeTest)
{
    type_test("fun(string, float)",
              type::function_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("string")},
                                                         type::identifier_t{ident_gen.ident("float")}),
                               t(type::tuple_t{{}})});
}

TEST_F(ParserTest, TypeTrailingCommaInParametersTest)
{
    type_test("fun(int, string, ): bool",
              type::function_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")},
                                                         type::identifier_t{ident_gen.ident("string")}),
                               t(type::identifier_t{ident_gen.ident("bool")})});
}

TEST_F(ParserTest, TypeGroupingTest)
{
    type_test("(fun(int): string)",
              type::function_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")}),
                               t(type::identifier_t{ident_gen.ident("string")})});
}

TEST_F(ParserTest, TypeFunctionPrecedenceTest)
{
    type_test(
        "fun(int[10], rec a: string end): arr[5]",
        type::function_t{make_vector<type::type_t>(
                             type::array_t{t(type::identifier_t{ident_gen.ident("int")}),
                                           e(expr::literal_t{token_t::l_integer("10")})},
                             type::record_t{make_vector2<type::record_t::field_t>(type::record_t::field_t{
                                 .name = ident_gen.ident("a"),
                                 .type = type::type_t{"", type::identifier_t{ident_gen.ident("string")}}})}),
                         t(type::array_t{t(type::identifier_t{ident_gen.ident("arr")}),
                                         e(expr::literal_t{token_t::l_integer("5")})})});
}

TEST_F(ParserTest, TypeFunctionArrayTest)
{
    type_test(
        "(fun(int):int)[3]",
        type::array_t{t(type::function_t{make_vector<type::type_t>(type::identifier_t{ident_gen.ident("int")}),
                                         t(type::identifier_t{ident_gen.ident("int")})}),
                      e(expr::literal_t{token_t::l_integer("3")})});
};

// TODO: Add negative testing cases!
