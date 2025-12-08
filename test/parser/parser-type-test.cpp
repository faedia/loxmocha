#include "helpers.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <vector>

using namespace loxmocha;
using namespace loxmocha::test::helpers;

TEST(ParserTest, TypeIdentifierTest) { type_test("my_type", type::identifier_t{token_t::k_identifier("my_type")}); }

TEST(ParserTest, TypeArrayTest)
{
    type_test("MyType[5]",
              type::array_t{t(type::identifier_t{token_t::k_identifier("MyType")}),
                            e(expr::literal_t{token_t::l_integer("5")})});
}

TEST(ParserTest, TypeMultiDimensionalArrayTest)
{
    type_test("MyType[3][const_func()]",
              type::array_t{
                  t(
                      type::array_t{t(type::identifier_t{token_t::k_identifier("MyType")}),
                                    e(expr::literal_t{token_t::l_integer("3")})}),
                  e(expr::call_t{
                      e(expr::identifier_t{token_t::k_identifier("const_func")}), {}, {}})});
}

TEST(ParserTest, TypeTupleTest)
{
    type_test("(int, string, arr[5])",
              type::tuple_t{make_vector<type::type_t>(
                  type::identifier_t{token_t::k_identifier("int")},
                  type::identifier_t{token_t::k_identifier("string")},
                  type::array_t{t(type::identifier_t{token_t::k_identifier("arr")}),
                                e(expr::literal_t{token_t::l_integer("5")})})});
}

TEST(ParserTest, TypeEmptyTupleTest) { type_test("()", type::tuple_t{{}}); }

TEST(ParserTest, TypeTupleSingleElementTest)
{
    type_test("(int,)", type::tuple_t{make_vector<type::type_t>(type::identifier_t{token_t::k_identifier("int")})});
}

TEST(ParserTest, TypeTupleTrailingCommaTest)
{
    type_test("(int, arr[10], string,)",
              type::tuple_t{make_vector<type::type_t>(
                  type::identifier_t{token_t::k_identifier("int")},
                  type::array_t{t(type::identifier_t{token_t::k_identifier("arr")}),
                                e(expr::literal_t{token_t::l_integer("10")})},
                  type::identifier_t{token_t::k_identifier("string")})});
}

TEST(ParserTest, TypeRecordTest)
{
    type_test(
        R"(
    rec
        a: int,
        b: string
    end
    )",
        type::record_t{make_vector<type::record_t::field_t>(
            type::record_t::field_t{.name = token_t::k_identifier("a"),
                                    .type = type::identifier_t{token_t::k_identifier("int")}},
            type::record_t::field_t{.name = token_t::k_identifier("b"),
                                    .type = type::identifier_t{token_t::k_identifier("string")}})});
}

TEST(ParserTest, TypeEmptyRecordTest) { type_test("rec end", type::record_t{{}}); }

TEST(ParserTest, TypeTaggedTest)
{
    type_test(R"(
    choice
        FirstTag: int,
        SecondTag: rec
            name: string,
            value: int
        end
    end
    )",
              type::tagged_t{make_vector<type::tagged_t::tag_t>(
                  type::tagged_t::tag_t{.name = token_t::k_identifier("FirstTag"),
                                        .type = type::identifier_t{token_t::k_identifier("int")}},
                  type::tagged_t::tag_t{
                      .name = token_t::k_identifier("SecondTag"),
                      .type = type::record_t{make_vector<type::record_t::field_t>(
                          type::record_t::field_t{.name = token_t::k_identifier("name"),
                                                  .type = type::identifier_t{token_t::k_identifier("string")}},
                          type::record_t::field_t{.name = token_t::k_identifier("value"),
                                                  .type = type::identifier_t{token_t::k_identifier("int")}})

                      }}

                  )});
}

TEST(ParserTest, TypeSingleTaggedTest)
{
    type_test("choice SingleTag: () end",
              type::tagged_t{make_vector<type::tagged_t::tag_t>(
                  type::tagged_t::tag_t{.name = token_t::k_identifier("SingleTag"), .type = type::tuple_t{{}}})});
}

TEST(ParserTest, TypeReferenceTest)
{
    type_test("let int",
              type::reference_t{t(type::identifier_t{token_t::k_identifier("int")})});
}

TEST(ParserTest, TypeMutableReferenceTest)
{
    type_test("var string",
              type::mutable_t{t(type::identifier_t{token_t::k_identifier("string")})});
}

TEST(ParserTest, TypeFunctionTest)
{
    type_test("fun(int, (string, float)): bool",
              type::function_t{make_vector<type::type_t>(type::identifier_t{token_t::k_identifier("int")},
                                                         type::tuple_t{make_vector<type::type_t>(
                                                             type::identifier_t{token_t::k_identifier("string")},
                                                             type::identifier_t{token_t::k_identifier("float")})}),
                               t(type::identifier_t{token_t::k_identifier("bool")})});
}

TEST(ParserTest, TypeFunctionNoParametersTest)
{
    type_test("fun(): int",
              type::function_t{std::vector<type::type_t>{},
                               t(type::identifier_t{token_t::k_identifier("int")})});
}

TEST(ParserTest, TypeFunctionNoReturnTypeTest)
{
    type_test("fun(string, float)",
              type::function_t{make_vector<type::type_t>(type::identifier_t{token_t::k_identifier("string")},
                                                         type::identifier_t{token_t::k_identifier("float")}),
                               t(type::tuple_t{{}})});
}

TEST(ParserTest, TypeTrailingCommaInParametersTest)
{
    type_test("fun(int, string, ): bool",
              type::function_t{make_vector<type::type_t>(type::identifier_t{token_t::k_identifier("int")},
                                                         type::identifier_t{token_t::k_identifier("string")}),
                               t(type::identifier_t{token_t::k_identifier("bool")})});
}

TEST(ParserTest, TypeGroupingTest)
{
    type_test("(fun(int): string)",
              type::function_t{make_vector<type::type_t>(type::identifier_t{token_t::k_identifier("int")}),
                               t(type::identifier_t{token_t::k_identifier("string")})});
}

TEST(ParserTest, TypeFunctionPrecedenceTest)
{
    type_test(
        "fun(int[10], rec a: string end): arr[5]",
        type::function_t{
            make_vector<type::type_t>(
                type::array_t{t(type::identifier_t{token_t::k_identifier("int")}),
                              e(expr::literal_t{token_t::l_integer("10")})},
                type::record_t{make_vector<type::record_t::field_t>(type::record_t::field_t{
                    .name = token_t::k_identifier("a"), .type = type::identifier_t{token_t::k_identifier("string")}})}),
            t(
                type::array_t{t(type::identifier_t{token_t::k_identifier("arr")}),
                              e(expr::literal_t{token_t::l_integer("5")})})});
}

TEST(ParserTest, TypeFunctionArrayTest)
{
    type_test("(fun(int):int)[3]",
              type::array_t{t(type::function_t{
                                make_vector<type::type_t>(type::identifier_t{token_t::k_identifier("int")}),
                                t(type::identifier_t{token_t::k_identifier("int")})}),
                            e(expr::literal_t{token_t::l_integer("3")})});
};

// TODO: Add negative testing cases!
