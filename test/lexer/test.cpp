#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/token.hpp"

#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

using namespace loxmocha;

class LexerTest : public testing::TestWithParam<std::tuple<std::string_view, token_t::kind_e, std::string>> {};

TEST_P(LexerTest, SingleTokenTest)
{
    const auto& [input, expected_kind, expected_span] = GetParam();

    auto result = loxmocha::lex(input);

    ASSERT_TRUE(result.has_value());

    auto value = result.value();

    EXPECT_EQ(value.kind(), expected_kind);
    EXPECT_EQ(value.span(), expected_span);
}

namespace {
// NOLINTNEXTLINE(cert-err58-cpp)
const std::vector<std::tuple<std::string_view, token_t::kind_e, std::string>> values = {
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define LOXMOCHA_TOKEN(name, example, value) std::make_tuple(example, token_t::kind_e::name, example),
#define LOXMOCHA_FAKE_TOKEN(name, example, value)
#include <loxmocha/ast/token.def>
};
} // namespace

INSTANTIATE_TEST_SUITE_P(SingleTokenTest, LexerTest, ::testing::ValuesIn(values));
