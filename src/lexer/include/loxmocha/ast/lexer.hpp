#pragma once

#include "token.hpp"

#include <expected>
#include <format>
#include <ostream>

namespace loxmocha {

/**
 * @class source_location_t
 * @brief Represents the source location of a token in the input stream.
 *
 */
class source_location_t {
public:
    source_location_t(std::string_view file, size_t line, size_t column) : file_(file), line_(line), column_(column) {}

    [[nodiscard]] constexpr auto file() const -> std::string_view { return file_; }
    [[nodiscard]] constexpr auto line() const -> size_t { return line_; }
    [[nodiscard]] constexpr auto column() const -> size_t { return column_; }

    [[nodiscard]] constexpr auto operator==(const source_location_t& other) const -> bool
    {
        return file_ == other.file_ && line_ == other.line_ && column_ == other.column_;
    }

private:
    std::string_view file_;
    size_t           line_;
    size_t           column_;
};

inline auto operator<<(std::ostream& os, const source_location_t& loc) -> std::ostream&
{
    return os << loc.file() << ":" << loc.line() << ":" << loc.column();
}

/**
 * @class lex_error_t
 * @brief Represents a lexical error encountered during lexing of the input stream.
 *
 */
class lex_error_t {
public:
    /**
     * @brief Enumeration representing the reason for a lexical error.
     */
    enum class reason_e : uint8_t {
        unknown_token_error,
        eof_error,
    };

    /**
     * @brief Constructs a lexical error with the specified reason, span, and location in the input stream.
     *
     * @param reason The reason for the lexical error.
     * @param span The span of the input stream that caused the error.
     * @param location The source location of the error in the input stream.
     */
    constexpr lex_error_t(reason_e reason, std::string_view span, const source_location_t& location)
        : reason_(reason), span_(span), location_(location)
    {
    }

    /**
     * @brief Get the reason for the lexical error.
     *
     * @return reason_e The reason for the lexical error.
     */
    [[nodiscard]] constexpr auto reason() const -> reason_e { return reason_; }
    /**
     * @brief Get the span of the input stream that caused the lexical error.
     *
     * @return std::string_view The span of the input stream that caused the error.
     */
    [[nodiscard]] constexpr auto span() const -> std::string_view { return span_; }
    /**
     * @brief Get the source location of the lexical error in the input stream.
     *
     * @return const source_location_t& The source location of the error in the input stream.
     */
    [[nodiscard]] constexpr auto location() const -> const source_location_t& { return location_; }

    /**
     * @brief Get a human-readable message describing the lexical error.
     */
    [[nodiscard]] constexpr auto message() const -> std::string
    {
        switch (reason_) {
        case reason_e::unknown_token_error:
            return std::format("{}:{}:{}: Unknown character '{}' in input",
                               location_.file(),
                               location_.line(),
                               location_.column(),
                               span_);
        case reason_e::eof_error:
            return std::format("{}:{}:{}: Unexpected end of file while processing '{}'",
                               location_.file(),
                               location_.line(),
                               location_.column(),
                               span_);
        default:
            return std::format("{}:{}:{}: Unknown lex error with input '{}'",
                               location_.file(),
                               location_.line(),
                               location_.column(),
                               span_);
        }
    }

private:
    reason_e          reason_;   // The reason for the lexical error.
    std::string_view  span_;     // The span of the input stream that caused the error.
    source_location_t location_; // The source location of the error in the input stream.
};

class lexer_t {
public:
    /**
     * @brief Constructs a lexer for a given input string.
     *
     * @param input The input string to be lexed.
     */
    explicit lexer_t(std::string_view input) : input_(input), current_iter_{input_.begin()}, current_token_{lex(input)}
    {
    }

    /**
     * @brief Get the next token from the input stream, and advance the lexer's current position on both success and
     * failure.
     *
     * If the lexer fails to lex a token, then the lexer returns a lexer_error_t and the next call to next_token() will
     * continue lexing from the position after the error.
     *
     * @return std::expected<token_t, lex_error_t> The next token from the input stream, or a lexical error if an error
     * occurs.
     */
    [[nodiscard]] auto next_token() -> std::expected<token_t, lex_error_t>;

    [[nodiscard]] auto peek_token() const -> std::expected<token_t, lex_error_t>;

    void consume_token();

    void reset_token(const token_t& token);

    /**
     * @brief Get the current position in the input stream.
     *
     * @return std::string_view::iterator The current position in the input stream.
     */
    [[nodiscard]] auto current_pos() const -> std::string_view::iterator { return current_iter_; }

private:
    std::string_view           input_;        // The input string to be lexed.
    std::string_view::iterator current_iter_; // The current position in the input string.
    std::expected<token_t, lex_error_t>
        current_token_; // The current token being processed. This is returned by peek_token().

    /**
     * @brief Lexes the input string and returns the next token.
     *
     * @param input The input string to be lexed.
     *
     * @return std::expected<token_t, lex_error_t> The next token from the input string, or a lexical error if an error.
     */
    [[nodiscard]] auto lex(std::string_view input) const -> std::expected<token_t, lex_error_t>;

    /**
     * @brief Get the source location of a character in the input stream.
     *
     * @param iter Iterator to get the source location for.
     * @return source_location_t The source location of the character in the input stream.
     */
    [[nodiscard]] auto source_location(std::string_view::iterator iter) const -> source_location_t;

    /**
     * @brief Lexes an identifier or keyword from the input stream.
     *
     * @param input The input string to lex an identifier or keyword from.
     * @return std::expected<token_t, lex_error_t> A token representing the identifier or keyword, or a lexical error if
     * not.
     */
    [[nodiscard]] auto lex_ident(std::string_view input) const -> std::expected<token_t, lex_error_t>;

    /**
     * @brief Lexes a character from the input stream.
     *
     * @param input The input string to lex a character from.
     * @return std::expected<token_t, lex_error_t> A token representing the character, or a lexical error if not.
     */
    [[nodiscard]] auto lex_char(std::string_view input) const -> std::expected<token_t, lex_error_t>;

    /**
     * @brief Lexes a string from the input stream.
     *
     * @param input The input string to lex a string from.
     * @return std::expected<token_t, lex_error_t> A token representing the string, or a lexical error if not.
     */
    [[nodiscard]] auto lex_string(std::string_view input) const -> std::expected<token_t, lex_error_t>;
};

} // namespace loxmocha
