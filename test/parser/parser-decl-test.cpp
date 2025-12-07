#include "assert_visitor.hpp"
#include "helpers.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"

#include "gtest/gtest.h"

using namespace loxmocha::test::helpers;

TEST(ParserTest, DeclTypeTest)
{
    using namespace loxmocha;
    lexer_t lexer{"type my_int is int"};
    parse_decl(lexer).result().visit(
        test::assert_visitor{},
        decl::type_t{token_t::k_identifier("my_int"), t(type::identifier_t{token_t::k_identifier("int")})});
}

TEST(ParserTest, DeclFunctionTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, DeclFunctionBlockBodyTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, DeclFunctionNoParametersTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, DeclFunctionNoReturnTypeTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, DeclVariableTest) { FAIL() << "Not implemented"; }

// TODO: Add negative testing cases!
