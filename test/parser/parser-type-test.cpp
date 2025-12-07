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

TEST(ParserTest, TypeArrayTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeMultiDimensionalArrayTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeTupleTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeEmptyTupleTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeTupleSingleElementTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeTupleTrailingCommaTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeRecordTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeEmptyRecordTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeTaggedTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeSingleTaggedTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeReferenceTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeMutableReferenceTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeFunctionTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeFunctionNoParametersTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeFunctionNoReturnTypeTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeTrailingCommaInParametersTest) { FAIL() << "Not implemented"; }

TEST(ParserTest, TypeGroupingTest) { FAIL() << "Not implemented"; }

// TODO: Add negative testing cases!
