#include "loxmocha/ast/lexer.hpp"

#include "loxmocha/ast/token.hpp"

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <expected>
#include <iterator>
#include <string_view>

namespace loxmocha {

namespace {
    /**
     * @brief Checks if the iterator points to the expected character and is not at the end of the input.
     *
     * @param expected The expected character to match.
     * @param iter The iterator check.
     * @param end The end iterator of the input string.
     * @return bool True if the iterator points to the expected character, otherwise False.
     */
    [[nodiscard]] auto match(char expected, std::string_view::iterator iter, std::string_view::iterator end) -> bool
    {
        return iter != end && *iter == expected;
    }

    /**
     * @brief Checks if the character is a valid start of an identifier.
     *
     * @param chr The character to check.
     * @return bool True if the character is a valid start of an identifier, otherwise False.
     */
    [[nodiscard]] auto is_ident_start(char chr) -> bool
    {
        return ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z');
    }

    /**
     * @brief Checks if the character is a valid identifier character.
     *
     * @param chr The character to check.
     * @return bool True if the character is a valid identifier character, otherwise False.
     */
    [[nodiscard]] auto is_ident_char(char chr) -> bool
    {
        return ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z') || ('0' <= chr && chr <= '9') || (chr == '_');
    }

    /**
     * @brief Lexes an integer from the input stream.
     *
     * @param input The input string to lex an integer from.
     * @return token_t A token representing the integer.
     */
    [[nodiscard]] auto lex_integer(std::string_view input) -> token_t
    {
        assert(!input.empty());
        assert(std::isdigit(input.front()) != 0);

        const auto* token_begin = input.begin();
        const auto* token_end =
            std::ranges::find_if_not(token_begin, input.end(), [](char chr) { return std::isdigit(chr) != 0; });
        return token_t::l_integer({token_begin, token_end});
    }

    /**
     * @brief Lexes a line comment from the input stream.
     *
     * @param input The input string to lex a line comment from.
     * @return token_t A token representing the line comment.
     */
    [[nodiscard]] auto lex_line_comment(std::string_view input) -> token_t
    {
        assert(input.size() >= 2);
        assert(input.starts_with("//"));

        const auto* end_iter = std::ranges::find(input.begin(), input.end(), '\n');
        return token_t::c_line_comment({input.begin(), end_iter});
    }

    /**
     * @brief Lexes a block comment from the input stream.
     *
     * @param input The input string to lex a block comment from.
     * @return token_t A token representing the block comment, or a lexical error if not.
     */
    [[nodiscard]] auto lex_block_comment(std::string_view input) -> std::expected<token_t, lex_error_t>
    {
        assert(input.size() >= 2);
        assert(input.starts_with("/*"));

        const auto* iter = std::next(input.begin(), 2);

        for (; iter != input.end(); iter = std::next(iter)) {
            if (*iter == '*' && match('/', std::next(iter), input.end())) {
                // Found the end of the block comment.
                return token_t::c_block_comment({input.begin(), std::next(iter, 2)});
            }
        }

        // If we reached here, then we are at the end of the input without finding the end of the block comment.
        // So we will assume that this is an unterminated block comment.
        // And will automatically end the block comment at the end of the input.
        return token_t::c_block_comment({input.begin(), input.end()});
    }
} // namespace

auto lexer_t::next_token() -> std::expected<token_t, lex_error_t>
{
    auto token = peek_token();
    if (token) {
        current_iter_ = token->span().end();
    } else {
        // Advance to past the token error.
        current_iter_ = token.error().span().end();
    }
    return token;
}

auto lexer_t::peek_token() const -> std::expected<token_t, lex_error_t>
{
    if (current_iter_ == input_.end()) {
        // If the current iterator is at the end of the input stream, return an EOF token.
        return token_t::s_eof({current_iter_, current_iter_});
    }

    return lex({current_iter_, input_.end()});
}

void lexer_t::consume_token() { [[maybe_unused]] auto token = next_token(); }

void lexer_t::reset_token(const token_t& token) { current_iter_ = token.span().begin(); }

auto lexer_t::source_location(std::string_view::iterator iter) const -> source_location_t
{
    auto        last_new_line = std::string_view{input_.begin(), iter}.find_last_of('\n');
    const auto* start_of_line =
        last_new_line == std::string_view::npos ? input_.begin() : input_.begin() + last_new_line + 1;

    const size_t line   = std::count(input_.begin(), iter, '\n') + 1;
    const size_t column = std::distance(start_of_line, iter) + 1;

    return source_location_t{"unknown_file", line, column};
}

auto lexer_t::lex(std::string_view input) const -> std::expected<token_t, lex_error_t>
{
    const auto* iter = input.begin();

    // Skip leading whitespace.
    for (; iter != input.end() && std::isspace(*iter) != 0; iter = std::next(iter)) {}

    // If we reached the end of the input, return an EOF token.
    if (iter == input.end()) {
        return token_t::s_eof({iter, iter});
    }

    // Here finally have a start of a token.
    const auto* token_begin = iter;

    switch (*iter) {
    case '(': return token_t::p_left_paren({token_begin, std::next(iter)});
    case ')': return token_t::p_right_paren({token_begin, std::next(iter)});
    case '[': return token_t::p_left_square({token_begin, std::next(iter)});
    case ']': return token_t::p_right_square({token_begin, std::next(iter)});
    case '{': return token_t::p_left_brace({token_begin, std::next(iter)});
    case '}': return token_t::p_right_brace({token_begin, std::next(iter)});
    case ':': return token_t::p_colon({token_begin, std::next(iter)});
    case ';': return token_t::p_semicolon({token_begin, std::next(iter)});
    case ',': return token_t::p_comma({token_begin, std::next(iter)});
    case '.': return token_t::p_period({token_begin, std::next(iter)});
    case '=': {
        iter = std::next(iter);
        if (match('=', iter, input.end())) {
            return token_t::p_equal_equal({token_begin, std::next(iter)});
        }
        if (match('>', iter, input.end())) {
            return token_t::p_arrow({token_begin, std::next(iter)});
        }
        return token_t::p_equal({token_begin, iter});
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
        if (match('/', iter, input.end())) {
            return lex_line_comment({std::prev(iter), input.end()});
        }
        if (match('*', iter, input.end())) {
            return lex_block_comment({std::prev(iter), input.end()});
        }
        return token_t::p_slash({token_begin, iter});
    }
    case '%': return token_t::p_percent({token_begin, std::next(iter)});
    case '^': return token_t::p_caret({token_begin, std::next(iter)});
    case '~': return token_t::p_tilde({token_begin, std::next(iter)});
    case '!': {
        iter = std::next(iter);
        if (match('=', iter, input.end())) {
            return token_t::p_not_equal({token_begin, std::next(iter)});
        }
        return token_t::p_bang({token_begin, iter});
    }
    case '\'': {
        return lex_char({token_begin, input.end()});
    }
    case '"': {
        return lex_string({token_begin, input.end()});
    }
    default: {
        if (std::isdigit(*iter) != 0) {
            return lex_integer({token_begin, input.end()});
        }
        return lex_ident({token_begin, input.end()});
    }
    }
}

auto lexer_t::lex_ident(std::string_view input) const -> std::expected<token_t, lex_error_t>
{
    const auto* iter        = input.begin();
    const auto* token_begin = iter;

    // If the first character is not a valid identifier start then we have a lexical error.
    if (!is_ident_start(*iter)) {
        return std::unexpected(
            lex_error_t{lex_error_t::reason_e::unknown_token_error, {iter, std::next(iter)}, source_location(iter)});
    }

    iter = std::next(iter);
    for (; iter != input.end() && is_ident_char(*iter); iter = std::next(iter)) {}

    return token_t::keyword_or_ident({token_begin, iter});
}

auto lexer_t::lex_char(std::string_view input) const -> std::expected<token_t, lex_error_t>
{
    assert(!input.empty());
    assert(input.starts_with('\''));

    const auto* token_begin = input.begin();
    const auto* iter        = std::next(token_begin);

    if (iter != input.end() && std::next(iter) != input.end() && *std::next(iter) == '\'' && *iter != '\\') {
        // This is a single character, so we will return it as a token.
        return token_t::l_char({token_begin, std::next(iter, 2)});
    }

    if (iter != input.end() && *iter == '\'') {
        // We have an empty character literal, which is an error.
        return std::unexpected(lex_error_t{
            lex_error_t::reason_e::unknown_token_error, {token_begin, std::next(iter)}, source_location(token_begin)});
    }

    for (; iter != input.end(); iter = std::next(iter)) {
        if (*iter == '\'') {
            // Found an end to the character literal.
            return token_t::l_char({token_begin, std::next(iter)});
        }
        if (*iter == '\\') {
            // This is an escape character, so we will skip the next character.
            iter = std::next(iter);
            if (iter == input.end()) {
                break; // The escape character is at the end of the input, so we will have an error.
            }
        }
    }

    return std::unexpected(
        lex_error_t{lex_error_t::reason_e::eof_error, {token_begin, iter}, source_location(token_begin)});
}

auto lexer_t::lex_string(std::string_view input) const -> std::expected<token_t, lex_error_t>
{
    assert(!input.empty());
    assert(input.starts_with('"'));

    const auto* token_begin = input.begin();
    const auto* iter        = std::next(token_begin);

    for (; iter != input.end(); iter = std::next(iter)) {
        if (*iter == '"') {
            // Found the end of the string.
            return token_t::l_string({token_begin, std::next(iter)});
        }
        if (*iter == '\\') {
            // This is an escape character, so we will skip the next character.
            iter = std::next(iter);
            if (iter == input.end()) {
                break; // The escape character is at the end of the input, so we will have an error.
            }
        }
    }

    // If we reached here, then we did not find the end of the string before the end of the input.
    return std::unexpected(
        lex_error_t{lex_error_t::reason_e::eof_error, {token_begin, iter}, source_location(token_begin)});
}

} // namespace loxmocha