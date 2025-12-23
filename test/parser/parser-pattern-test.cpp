#include "assert_visitor.hpp"
#include "helpers.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"

#include "gtest/gtest.h"

using namespace loxmocha::ast;
using namespace loxmocha::lexer;
using namespace loxmocha::test::helpers;

TEST_F(ParserTest, PatternIdentifierTest)
{
    using namespace loxmocha;
    lexer_t lexer{"my_var"};
    parse_pattern(lexer).result().visit(test::assert_visitor{}, pattern::identifier_t{ident_gen.ident("my_var")});
}

TEST_F(ParserTest, PatternTagTest)
{
    using namespace loxmocha;
    lexer_t lexer{"choice SomeTagType.SomeTagName my_var"};
    parse_pattern(lexer).result().visit(test::assert_visitor{},
                                        pattern::tag_t{t(type::identifier_t{ident_gen.ident("SomeTagType")}),
                                                       ident_gen.ident("SomeTagName"),
                                                       p(pattern::identifier_t{ident_gen.ident("my_var")})});
}

TEST_F(ParserTest, PatternTagInTagTest)
{
    using namespace loxmocha;
    lexer_t lexer{"choice SomeTagType.SomeTagName (choice AnotherTagType.AnotherTagName inner_var)"};
    parse_pattern(lexer).result().visit(
        test::assert_visitor{},
        pattern::tag_t{t(type::identifier_t{ident_gen.ident("SomeTagType")}),
                       ident_gen.ident("SomeTagName"),
                       p(pattern::tag_t{t(type::identifier_t{ident_gen.ident("AnotherTagType")}),
                                        ident_gen.ident("AnotherTagName"),
                                        p(pattern::identifier_t{ident_gen.ident("inner_var")})})});
}

// TODO: Add negative testing cases!
