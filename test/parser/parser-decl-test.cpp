#include "helpers.hpp"
#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <vector>

using namespace loxmocha;
using namespace loxmocha::ast;
using namespace loxmocha::lexer;
using namespace loxmocha::test::helpers;

TEST_F(ParserTest, DeclTypeTest)
{
    decl_test("type my_int is int",
              decl::type_t{ident_gen.ident("my_int"), t(type::identifier_t{ident_gen.ident("int")})});
}

TEST_F(ParserTest, DeclFunctionTest)
{
    decl_test(
        "fun my_func(a: int, b: string): bool => to_string(a) == b",
        decl::function_t{
            ident_gen.ident("my_func"),
            make_vector2<decl::function_t::parameter_t>(
                decl::function_t::parameter_t{.name = ident_gen.ident("a"),
                                              .type = type::type_t{"", type::identifier_t{ident_gen.ident("int")}}},
                decl::function_t::parameter_t{.name = ident_gen.ident("b"),
                                              .type = type::type_t{"", type::identifier_t{ident_gen.ident("string")}}}

                ),
            t(type::identifier_t{ident_gen.ident("bool")}),
            e(expr::binary_t{token_t::p_equal_equal("=="),
                             e(expr::call_t{e(expr::identifier_t{ident_gen.ident("to_string")}),
                                            make_vector<expr::expr_t>(expr::identifier_t{ident_gen.ident("a")}),
                                            {}}),
                             e(expr::identifier_t{ident_gen.ident("b")})})

        });
}

TEST_F(ParserTest, DeclFunctionBlockBodyTest)
{
    decl_test(
        R"(
        fun my_func(n : int): int begin
            var a: int = 0;
            let b: int = 1;
            a = a + b;
            a * b
        end
    )",
        decl::function_t{
            ident_gen.ident("my_func"),
            make_vector2<decl::function_t::parameter_t>(decl::function_t::parameter_t{
                .name = ident_gen.ident("n"), .type = type::type_t{"", type::identifier_t{ident_gen.ident("int")}}}),
            t(type::identifier_t{ident_gen.ident("int")}),
            e(expr::block_t{make_vector<stmt::stmt_t>(
                                stmt::decl_t{d(decl::variable_t{decl::variable_t::mut_e::var,
                                                                ident_gen.ident("a"),
                                                                t(type::identifier_t{ident_gen.ident("int")}),
                                                                e(expr::literal_t{token_t::l_integer("0")})})},
                                stmt::decl_t{d(decl::variable_t{decl::variable_t::mut_e::let,
                                                                ident_gen.ident("b"),
                                                                t(type::identifier_t{ident_gen.ident("int")}),
                                                                e(expr::literal_t{token_t::l_integer("1")})})},
                                stmt::assign_t{e(expr::identifier_t{ident_gen.ident("a")}),
                                               e(expr::binary_t{token_t::p_plus("+"),
                                                                e(expr::identifier_t{ident_gen.ident("a")}),
                                                                e(expr::identifier_t{ident_gen.ident("b")})})}),
                            e(expr::binary_t{token_t::p_asterisk("*"),
                                             e(expr::identifier_t{ident_gen.ident("a")}),
                                             e(expr::identifier_t{ident_gen.ident("b")})})})});
}

TEST_F(ParserTest, DeclFunctionNoParametersTest)
{
    decl_test("fun my_func(): int => 42",
              decl::function_t{ident_gen.ident("my_func"),
                               std::vector<decl::function_t::parameter_t>{},
                               t(type::identifier_t{ident_gen.ident("int")}),
                               e(expr::literal_t{token_t::l_integer("42")})});
}

TEST_F(ParserTest, DeclFunctionNoReturnTypeTest)
{
    decl_test("fun my_func(a: int) => print(a)",
              decl::function_t{ident_gen.ident("my_func"),
                               make_vector2<decl::function_t::parameter_t>(decl::function_t::parameter_t{
                                   .name = ident_gen.ident("a"),
                                   .type = type::type_t{"", type::identifier_t{ident_gen.ident("int")}}}),
                               t(type::tuple_t{{}}),
                               e(expr::call_t{e(expr::identifier_t{ident_gen.ident("print")}),
                                              make_vector<expr::expr_t>(expr::identifier_t{ident_gen.ident("a")}),
                                              {}})});
}

TEST_F(ParserTest, DeclLetTest)
{
    decl_test("let my_var: int[10][5] = create_array()",
              decl::variable_t{decl::variable_t::mut_e::let,
                               ident_gen.ident("my_var"),
                               t(type::array_t{t(type::array_t{t(type::identifier_t{ident_gen.ident("int")}),
                                                               e(expr::literal_t{token_t::l_integer("10")})}),
                                               e(expr::literal_t{token_t::l_integer("5")})}),
                               e(expr::call_t{e(expr::identifier_t{ident_gen.ident("create_array")}), {}, {}})});
}

TEST_F(ParserTest, DeclVarTest)
{
    decl_test("var counter: int = 0",
              decl::variable_t{decl::variable_t::mut_e::var,
                               ident_gen.ident("counter"),
                               t(type::identifier_t{ident_gen.ident("int")}),
                               e(expr::literal_t{token_t::l_integer("0")})});
}

// TODO: Add negative testing cases!
