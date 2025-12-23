#include "helpers.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"

#include "gtest/gtest.h"

using namespace loxmocha;
using namespace loxmocha::ast;
using namespace loxmocha::lexer;
using namespace loxmocha::test::helpers;

TEST_F(ParserTest, StmtExprTest) { stmt_test("42", stmt::expr_t{e(expr::literal_t{token_t::l_integer("42")})}); }

TEST_F(ParserTest, StmtAssignTest)
{
    stmt_test("my_var = my_func(10) + 500",
              stmt::assign_t{
                  e(expr::identifier_t{ident_gen.ident("my_var")}),
                  e(expr::binary_t{token_t::p_plus("+"),
                                   e(expr::call_t{e(expr::identifier_t{ident_gen.ident("my_func")}),
                                                  make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("10")}),
                                                  {}}),
                                   e(expr::literal_t{token_t::l_integer("500")})})});
}

TEST_F(ParserTest, StmtComplexTargetAssignTest)
{
    stmt_test(
        "my_array[2 + index].field = another_func(arg1, arg2)",
        stmt::assign_t{
            e(expr::field_t{e(expr::index_t{e(expr::identifier_t{ident_gen.ident("my_array")}),
                                            e(expr::binary_t{token_t::p_plus("+"),
                                                             e(expr::literal_t{token_t::l_integer("2")}),
                                                             e(expr::identifier_t{ident_gen.ident("index")})})}),
                            ident_gen.ident("field")}),
            e(expr::call_t{e(expr::identifier_t{ident_gen.ident("another_func")}),
                           make_vector<expr::expr_t>(

                               expr::identifier_t{ident_gen.ident("arg1")},
                               expr::identifier_t{ident_gen.ident("arg2")}),
                           {}})});
}

TEST_F(ParserTest, StmtDeclTest)
{
    stmt_test("let my_var: int = 100",
              stmt::decl_t{d(decl::variable_t{decl::variable_t::mut_e::let,
                                              ident_gen.ident("my_var"),
                                              t(type::identifier_t{ident_gen.ident("int")}),
                                              e(expr::literal_t{token_t::l_integer("100")})})});
}

// TODO: Add negative testing cases!
