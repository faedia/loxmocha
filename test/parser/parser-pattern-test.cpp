#include "assert_visitor.hpp"
#include "helpers.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/token.hpp"

#include "gtest/gtest.h"

using namespace loxmocha::test::helpers;

TEST(ParserTest, PatternIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"my_var"};
    parse_pattern(lexer).result().visit(test::assert_visitor{}, pattern::identifier_t{token_t::k_identifier("my_var")});
}

TEST(ParserTest, PatternTagTest)
{
    using namespace loxmocha;
    lexer_t lexer{"choice SomeTagType.SomeTagName my_var"};
    parse_pattern(lexer).result().visit(test::assert_visitor{},
                                        pattern::tag_t{t(type::identifier_t{token_t::k_identifier("SomeTagType")}),
                                                       token_t::k_identifier("SomeTagName"),
                                                       p(pattern::identifier_t{token_t::k_identifier("my_var")})});
}

TEST(ParserTest, PatternTagInTagTest)
{
    using namespace loxmocha;
    lexer_t lexer{"choice SomeTagType.SomeTagName (choice AnotherTagType.AnotherTagName inner_var)"};
    parse_pattern(lexer).result().visit(
        test::assert_visitor{},
        pattern::tag_t{t(type::identifier_t{token_t::k_identifier("SomeTagType")}),
                       token_t::k_identifier("SomeTagName"),
                       p(pattern::tag_t{t(type::identifier_t{token_t::k_identifier("AnotherTagType")}),
                                        token_t::k_identifier("AnotherTagName"),
                                        p(pattern::identifier_t{token_t::k_identifier("inner_var")})})});
}

// TODO: Add negative testing cases!
