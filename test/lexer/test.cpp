#include "lexer/lexer.hpp"

#include <gtest/gtest.h>

TEST(LexerTest, InitialTest)
{
    loxmocha::lexer();

    EXPECT_EQ(1, 1);
}
