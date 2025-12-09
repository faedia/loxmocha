#include "helpers.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"

using namespace loxmocha;
using namespace loxmocha::test::helpers;

TEST(ParserTest, StmtExprTest)
{
    stmt_test("42", stmt::expr_t{safe_ptr<expr::expr_t>::make(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, StmtAssignTest)
{
    stmt_test("my_var = my_func(10) + 500",
              stmt::assign_t{
                  e(expr::identifier_t{token_t::k_identifier("my_var")}),
                  e(expr::binary_t{token_t::p_plus("+"),
                                   e(expr::call_t{e(expr::identifier_t{token_t::k_identifier("my_func")}),
                                                  make_vector<expr::expr_t>(expr::literal_t{token_t::l_integer("10")}),
                                                  {}}),
                                   e(expr::literal_t{token_t::l_integer("500")})

                  })});
}

TEST(ParserTest, StmtComplexTargetAssignTest)
{
    stmt_test(
        "my_array[2 + index].field = another_func(arg1, arg2)",
        stmt::assign_t{
            e(expr::field_t{e(expr::index_t{e(expr::identifier_t{token_t::k_identifier("my_array")}),
                                            e(expr::binary_t{token_t::p_plus("+"),
                                                             e(expr::literal_t{token_t::l_integer("2")}),
                                                             e(expr::identifier_t{token_t::k_identifier("index")})})

                            }),
                            token_t::k_identifier("field")}),
            e(expr::call_t{e(expr::identifier_t{token_t::k_identifier("another_func")}),
                           make_vector<expr::expr_t>(

                               expr::identifier_t{token_t::k_identifier("arg1")},
                               expr::identifier_t{token_t::k_identifier("arg2")}),
                           {}

            })});
}

TEST(ParserTest, StmtDeclTest)
{
    stmt_test("let my_var: int = 100",
              stmt::decl_t{d(decl::variable_t{decl::variable_t::mut_e::let,
                                              token_t::k_identifier("my_var"),
                                              t(type::identifier_t{token_t::k_identifier("int")}),
                                              e(expr::literal_t{token_t::l_integer("100")})})});
}

// TODO: Add negative testing cases!
