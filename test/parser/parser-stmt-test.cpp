#include "assert_visitor.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"

TEST(ParserTest, StmtExprTest)
{
    using namespace loxmocha;
    lexer_t lexer{"42"};
    parse_stmt(lexer).result().visit(
        test::assert_visitor{}, stmt::expr_t{safe_ptr<expr::expr_t>::make(expr::literal_t{token_t::l_integer("42")})});
}

TEST(ParserTest, StmtAssignTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, StmtComplexTargetAssignTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, StmtDeclTest) { FAIL() << "Not implemented"; }

// TODO: Add negative testing cases!
