#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/token.hpp"

#include "gmock/gmock.h"
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

using namespace loxmocha;
using namespace loxmocha::lexer;

class LexerTest
    : public testing::TestWithParam<std::tuple<std::string_view, token_t::kind_e, std::string, std::string>> {};

TEST_P(LexerTest, SingleTokenTest)
{
    // Params for the test are:
    // - input: The input string to lex.
    // - expected_kind: The expected kind of the token.
    // - expected_span: The expected span of the token in the input string.
    // - _: A string used for the name of the test case.
    const auto& [input, expected_kind, expected_span, _] = GetParam();

    // Lex the first token in the input string.
    auto ident_map = ident_map_t{};
    auto lexer     = lexer_t{input, ident_map};
    auto result    = lexer.next_token();

    // Check if the result is a valid token.
    ASSERT_TRUE(result.has_value()) << "Failed to lex token: " << result.error().message();

    auto value = result.value();

    // Check if the token kind and span match the expected values.
    EXPECT_EQ(value.kind(), expected_kind);
    EXPECT_EQ(value.span(), expected_span);
}

namespace {
// NOLINTNEXTLINE(cert-err58-cpp)
const std::vector<std::tuple<std::string_view, token_t::kind_e, std::string, std::string>> simpleTokenTests = {
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define LOXMOCHA_TOKEN(name, example, value) std::make_tuple(example, token_t::kind_e::name, example, #name "_basic"),
#define LOXMOCHA_FAKE_TOKEN(name, example, value)
#include <loxmocha/ast/token.def>

    // Extra test cases for identifiers
    std::make_tuple("A_Capital_Ident", token_t::kind_e::k_identifier, "A_Capital_Ident", "k_identifier_capital"),
    std::make_tuple("ident_123_num", token_t::kind_e::k_identifier, "ident_123_num", "k_identifier_num"),
    std::make_tuple("ident_ended_with_space ",
                    token_t::kind_e::k_identifier,
                    "ident_ended_with_space",
                    "k_identifier_ended_with_space"),

    // Extra test cases for characters
    std::make_tuple("'\\''", token_t::kind_e::l_char, "'\\''", "l_char_quoted"),
    std::make_tuple("'multi char char'", token_t::kind_e::l_char, "'multi char char'", "l_char_multi_char"),

    // Extra test cases for strings
    std::make_tuple(
        R"("Hello, \"World\"")", token_t::kind_e::l_string, R"("Hello, \"World\"")", "l_string_empty_quoted"),

    // Extra test cases for block comments
    std::make_tuple("/* this is an unterminated comment",
                    token_t::kind_e::c_block_comment,
                    "/* this is an unterminated comment",
                    "l_block_comment_unterminated"),

    // End of file test cases
    std::make_tuple("", token_t::kind_e::s_eof, "", "k_eof_empty"),
    std::make_tuple(" ", token_t::kind_e::s_eof, "", "k_eof_whitespace"),
};
} // namespace

INSTANTIATE_TEST_SUITE_P(SingleTokenTest,
                         LexerTest,
                         ::testing::ValuesIn(simpleTokenTests),
                         [](const testing::TestParamInfo<LexerTest::ParamType>& info) -> std::string {
                             return std::get<3>(info.param);
                         });

class LexerErrorTest
    : public testing::TestWithParam<
          std::tuple<std::string_view, lex_error_t::reason_e, source_location_t, std::string, std::string>> {};

TEST_P(LexerErrorTest, LexErrorTest)
{
    // Params for the test are:
    // - input: The input string to lex.
    // - expected_reason: The expected reason for the lexical error.
    // - expected_location: The expected source location of the error in the input stream.
    // - expected_span: The expected span of the input stream that caused the error.
    // - _: A string used for the name of the test case.
    const auto& [input, expected_reason, expected_location, expected_span, _] = GetParam();

    // Lex the first token in the input string.
    auto ident_map = ident_map_t{};
    auto lexer     = lexer_t{input, ident_map};
    auto result    = lexer.next_token();

    // Check if the result is an error.
    ASSERT_FALSE(result.has_value()) << "Expected a lexical error, but got a token: " << result.value();

    auto error = result.error();

    // Check if the error reason, location, and span match the expected values.
    EXPECT_EQ(error.reason(), expected_reason);
    EXPECT_EQ(error.location(), expected_location);
    EXPECT_EQ(error.span(), expected_span);
}

namespace {
const std::vector<std::tuple<std::string_view, lex_error_t::reason_e, source_location_t, std::string, std::string>>
    // NOLINTNEXTLINE(cert-err58-cpp)
    simpleErrorTests = {
        // Unknown token error
        std::make_tuple("?",
                        lex_error_t::reason_e::unknown_token_error,
                        source_location_t{"unknown_file", 1, 1},
                        "?",
                        "unknown_token_error"),

        // EOF after char literal openning quote.
        std::make_tuple(
            "'", lex_error_t::reason_e::eof_error, source_location_t{"unknown_file", 1, 1}, "'", "eof_error_char"),

        // Empty char literal.
        std::make_tuple("''",
                        lex_error_t::reason_e::unknown_token_error,
                        source_location_t{"unknown_file", 1, 1},
                        "''",
                        "unknown_token_empty_char"),

        // EOF after char literal openning quote.
        std::make_tuple(R"('\)",
                        lex_error_t::reason_e::eof_error,
                        source_location_t{"unknown_file", 1, 1},
                        R"('\)",
                        "eof_error_char_escaped"),

        // EOF after char literal openning quote with contents.
        std::make_tuple("'a",
                        lex_error_t::reason_e::eof_error,
                        source_location_t{"unknown_file", 1, 1},
                        "'a",
                        "eof_error_char_unterminated"),

        // EOF while string literal is open
        std::make_tuple(R"("unterminated string)",
                        lex_error_t::reason_e::eof_error,
                        source_location_t{"unknown_file", 1, 1},
                        R"("unterminated string)",
                        "eof_error_string_unterminated"),

        // EOF after escape character in string literal.
        std::make_tuple(R"("unterminated escape \)",
                        lex_error_t::reason_e::eof_error,
                        source_location_t{"unknown_file", 1, 1},
                        R"("unterminated escape \)",
                        "eof_error_string_escape_unterminated"),

        // Token error on a different line and column.
        std::make_tuple(R"(
            ?)",
                        lex_error_t::reason_e::unknown_token_error,
                        source_location_t{"unknown_file", 2, 13},
                        "?",
                        "unknown_token_error_multiline"),

};
} // namespace

INSTANTIATE_TEST_SUITE_P(SingleTokenErrorTest,
                         LexerErrorTest,
                         ::testing::ValuesIn(simpleErrorTests),
                         [](const testing::TestParamInfo<LexerErrorTest::ParamType>& info) -> std::string {
                             return std::get<4>(info.param);
                         });

class LexerMultiTokenTest
    : public ::testing::TestWithParam<std::tuple<std::string_view, std::vector<token_t>, std::string>> {};

TEST_P(LexerMultiTokenTest, MultiTokenTest)
{
    const auto& [input, expected_tokens, _] = GetParam();

    // Lex the input string.
    auto ident_map = ident_map_t{};
    auto lexer     = lexer_t{input, ident_map};

    std::vector<token_t> tokens{};

    while (tokens.empty() || tokens.back().kind() != token_t::kind_e::s_eof) {
        auto token = lexer.next_token();

        ASSERT_TRUE(token.has_value()) << "Failed to lex token: " << token.error().message();

        tokens.push_back(token.value());
    }

    EXPECT_THAT(tokens,
                ::testing::Pointwise(::testing::Truly([](const auto& pair) {
                                         return std::get<0>(pair).kind() == std::get<1>(pair).kind()
                                                && std::get<0>(pair).span() == std::get<1>(pair).span();
                                     }),
                                     expected_tokens));
}

INSTANTIATE_TEST_SUITE_P(MultiTokenTest,
                         LexerMultiTokenTest,
                         ::testing::Values(std::make_tuple("1 + 2",
                                                           std::vector<token_t>{token_t::l_integer("1"),
                                                                                token_t::p_plus("+"),
                                                                                token_t::l_integer("2"),
                                                                                token_t::s_eof("")},
                                                           "multi_token_basic"),

                                           std::make_tuple("if (x > 0) { x }",
                                                           std::vector<token_t>{token_t::k_if("if"),
                                                                                token_t::p_left_paren("("),
                                                                                token_t::k_identifier("x", ident_t{0}),
                                                                                token_t::p_greater(">"),
                                                                                token_t::l_integer("0"),
                                                                                token_t::p_right_paren(")"),
                                                                                token_t::p_left_brace("{"),
                                                                                token_t::k_identifier("x", ident_t{0}),
                                                                                token_t::p_right_brace("}"),
                                                                                token_t::s_eof("")},
                                                           "multi_token_if_statement")),
                         [](const testing::TestParamInfo<LexerMultiTokenTest::ParamType>& info) -> std::string {
                             return std::get<2>(info.param);
                         });
