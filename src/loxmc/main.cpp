#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/token.hpp"

#include <expected>
#include <format>
#include <print>

auto main() -> int
{
    auto rename = [](std::expected<loxmocha::token_t, loxmocha::lex_error_t> tok) {
        if (tok) {
            return std::format("Success: \"{}\"", tok->span());
        }
        return tok.error().message();
    };

    std::println("{}", rename(loxmocha::lexer_t{"("}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{"="}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{"=>"}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{"=="}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{"=$"}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{"          \t =$"}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{"$"}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{" my_identifier"}.next_token()));
    std::println("{}", rename(loxmocha::lexer_t{" my_identifier "}.next_token()));
    return 0;
}
