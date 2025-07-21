#pragma once

#include <string>
#include <expected>

#include "token.hpp"

namespace loxmocha {

[[nodiscard]] constexpr auto match(char expected, std::string_view::iterator iter, std::string_view::iterator end)
    -> bool
{
    return iter != end && *iter == expected;
}

[[nodiscard]] constexpr auto is_ident_start(char chr) -> bool
{
    return ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z');
}

[[nodiscard]] constexpr auto is_ident_char(char chr) -> bool
{
    return ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z') || ('0' <= chr && chr <= '9') || (chr == '_');
}

[[nodiscard]] constexpr auto lex_ident(std::string_view input) -> std::expected<token_t, std::string>
{
    const auto* iter        = input.begin();
    const auto* token_begin = iter;
    if (!is_ident_start(*iter)) {
        return std::unexpected("Token error");
    }
    iter = std::next(iter);
    for (; iter != input.end() && is_ident_char(*iter); iter = std::next(iter)) {}

    return token_t::keyword_or_ident({token_begin, iter});
}

[[nodiscard]] constexpr auto lex_line_comment(std::string_view input) -> std::expected<token_t, std::string>
{
    if (!input.starts_with("//")) {
        return std::unexpected("Token error");
    }

    const auto* end_iter = std::ranges::find(input.begin(), input.end(), '\n');

    return token_t::c_line_comment({input.begin(), end_iter});
}

[[nodiscard]] constexpr auto lex(std::string_view input) -> std::expected<token_t, std::string>
{
    const auto* iter = input.begin();

    for (; iter != input.end() && std::isspace(*iter) != 0; iter = std::next(iter)) {}

    if (iter == input.end()) {
        return token_t::s_eof({iter, iter});
    }

    const auto* token_begin = iter;

    switch (*iter) {
    case '(': return token_t::p_left_paren({token_begin, std::next(iter)});
    case ')': return token_t::p_right_paren({token_begin, std::next(iter)});
    case '[': return token_t::p_left_square({token_begin, std::next(iter)});
    case ']': return token_t::p_right_square({token_begin, std::next(iter)});
    case '{': return token_t::p_left_brace({token_begin, std::next(iter)});
    case '}': return token_t::p_right_brace({token_begin, std::next(iter)});
    case ':': return token_t::p_colon({token_begin, std::next(iter)});
    case ',': return token_t::p_comma({token_begin, std::next(iter)});
    case '.': return token_t::p_period({token_begin, std::next(iter)});
    case '=': {
        iter = std::next(iter);
        if (match('=', iter, input.end())) {
            return token_t::p_equal({token_begin, std::next(iter)});
        }
        if (match('>', iter, input.end())) {
            return token_t::p_arrow({token_begin, std::next(iter)});
        }
        return token_t::p_assign({token_begin, iter});
    }
    case '>': {
        iter = std::next(iter);
        if (match('=', iter, input.end())) {
            return token_t::p_greater_equal({token_begin, std::next(iter)});
        }
        if (match('>', iter, input.end())) {
            return token_t::p_right_shift({token_begin, std::next(iter)});
        }
        return token_t::p_greater({token_begin, iter});
    }
    case '<': {
        iter = std::next(iter);
        if (match('=', iter, input.end())) {
            return token_t::p_less_equal({token_begin, std::next(iter)});
        }
        if (match('<', iter, input.end())) {
            return token_t::p_left_shift({token_begin, std::next(iter)});
        }
        return token_t::p_less({token_begin, iter});
    }
    case '&': {
        iter = std::next(iter);
        if (match('&', iter, input.end())) {
            return token_t::p_and_and({token_begin, std::next(iter)});
        }
        return token_t::p_and({token_begin, iter});
    }
    case '|': {
        iter = std::next(iter);
        if (match('|', iter, input.end())) {
            return token_t::p_pipe_pipe({token_begin, std::next(iter)});
        }
        return token_t::p_pipe({token_begin, iter});
    }
    case '+': return token_t::p_plus({token_begin, std::next(iter)});
    case '-': return token_t::p_minus({token_begin, std::next(iter)});
    case '*': return token_t::p_asterisk({token_begin, std::next(iter)});
    case '/': {
        iter = std::next(iter);
        switch (*iter) {
        case '/': return lex_line_comment({std::prev(iter), input.end()});
        case '*': // TODO: Parse block comment
        default: return token_t::p_slash({token_begin, iter});
        }
    }
    case '%': return token_t::p_percent({token_begin, std::next(iter)});
    case '^': return token_t::p_caret({token_begin, std::next(iter)});
    case '~': return token_t::p_tilde({token_begin, std::next(iter)});
    case '!': {
        iter = std::next(iter);
        switch (*iter) {
        case '=': return token_t::p_not_equal({token_begin, std::next(iter)});
        default: return token_t::p_bang({token_begin, iter});
        }
    }
    default: {
        return lex_ident({token_begin, input.end()});
    }
    }
}
} // namespace loxmocha
