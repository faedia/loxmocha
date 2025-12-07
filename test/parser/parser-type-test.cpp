#include "assert_visitor.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"

TEST(ParserTest, TypeIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"my_var"};
    parse_type(lexer).result().visit(test::assert_visitor{}, type::identifier_t{token_t::k_identifier("my_var")});
}
