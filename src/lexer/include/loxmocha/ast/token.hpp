#pragma once

#include "loxmocha/ast/ident_map.hpp"

#include <cstdint>
#include <format>
#include <ostream>
#include <string_view>
#include <unordered_map>

namespace loxmocha::lexer {

/**
 * @class token_t
 * @brief Represents a token in the LoxMocha language.
 *
 */
class token_t {
public:
    /**
     * @brief Enum representing all possible token kinds.
     */
    enum class kind_e : std::uint8_t {
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define LOXMOCHA_TOKEN(name, example, value) name = (value),
#include "loxmocha/ast/token.def"
    };

    /**
     * @brief Get the kind of the token.
     *
     * @return kind_e The kind of the token.
     */
    [[nodiscard]] constexpr auto kind() const -> kind_e { return kind_; }
    /**
     * @brief Gets the span of the token in in the input stream.
     *
     * @return std::string_view The span of the token.
     */
    [[nodiscard]] constexpr auto span() const -> std::string_view { return span_; }
    /**
     * @brief Gets the identifier ID of the token, if it is an identifier.
     *
     * @return identifier_t The identifier ID of the token.
     */
    [[nodiscard]] constexpr auto ident_id() const -> ident_t { return ident_id_; }

// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define LOXMOCHA_TOKEN(name, example, value)                                   \
    [[nodiscard]] constexpr static auto name(std::string_view span) -> token_t \
    {                                                                          \
        return token_t{kind_e::name, span, ident_t{}};                         \
    }
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define LOXMOCHA_IDENT(name, example, value)                                               \
    [[nodiscard]] constexpr static auto name(std::string_view span, ident_t id) -> token_t \
    {                                                                                      \
        return token_t{kind_e::name, span, id};                                            \
    }
#include "loxmocha/ast/token.def"

    token_t() = delete;

    /**
     * @brief Get the enumeration name of the token kind.
     *
     * @param kind The kind of the token.
     * @return const char* The name of the token kind.
     */
    [[nodiscard]] constexpr static auto kind_name(kind_e kind) -> const char*
    {
        switch (kind) {
#define LOXMOCHA_TOKEN(name, example, value) \
    case kind_e::name: return #name;
#include "loxmocha/ast/token.def"
        default: return "unknown";
        }
    }

    /**
     * @breif Make a keyword token from the given span.
     * @param ident The keyword string.
     * @return token_t The keyword token.
     */
    [[nodiscard]] constexpr static auto keyword_token(std::string_view ident) -> std::optional<token_t>
    {
        auto iter = keyword_kind.find(ident);
        if (iter != keyword_kind.end()) {
            return token_t{iter->second, ident, ident_t{0}};
        }
        return std::nullopt;
    }

private:
    /**
     * @brief Constructs a token with the specified kind and span.
     *
     * @param kind The kind of the token.
     * @param span The span of the token in the input stream.
     */
    constexpr token_t(kind_e kind, std::string_view span, ident_t id) : kind_{kind}, span_{span}, ident_id_{id} {}

    kind_e           kind_;     // The kind of the token.
    std::string_view span_;     // The span of the token in the input stream.
    ident_t          ident_id_; // The ID of the identifer, if the token is an identifier.

    // NOLINTNEXTLINE(cert-err58-cpp)
    static inline const std::unordered_map<std::string_view, kind_e> keyword_kind = {
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define LOXMOCHA_KEYWORD(name, example, value) {example, kind_e::name},
#include "loxmocha/ast/token.def"
    };
};

inline auto operator<<(std::ostream& stream, token_t::kind_e kind) -> std::ostream&
{
    stream << token_t::kind_name(kind);
    return stream;
}

inline auto operator<<(std::ostream& stream, const token_t& token) -> std::ostream&
{
    stream << "{ kind: " << token.kind() << ", span: \"" << token.span() << "\" }";
    return stream;
}

} // namespace loxmocha::lexer

template<>
struct std::formatter<loxmocha::lexer::token_t::kind_e> {
    static constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

    static auto format(loxmocha::lexer::token_t::kind_e kind, std::format_context& ctx)
    {
        return std::format_to(ctx.out(), "{}", loxmocha::lexer::token_t::kind_name(kind));
    }
};
