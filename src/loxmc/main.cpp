#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/token.hpp"

#include <expected>
#include <format>
#include <print>
#include <string>

auto main() -> int
{
    auto rename = [](std::expected<loxmocha::token_t, std::string> tok) {
        if (tok) {
            return std::format("Success: \"{}\"", tok->span());
        }
        return tok.error();
    };
    std::println("{}", rename(loxmocha::lex("(")));
    std::println("{}", rename(loxmocha::lex("=")));
    std::println("{}", rename(loxmocha::lex("=>")));
    std::println("{}", rename(loxmocha::lex("==")));
    std::println("{}", rename(loxmocha::lex("=$")));
    std::println("{}", rename(loxmocha::lex("          \t =$")));
    std::println("{}", rename(loxmocha::lex("$")));
    std::println("{}", rename(loxmocha::lex(" my_identifier")));
    std::println("{}", rename(loxmocha::lex(" my_identifier ")));
    return 0;
}
