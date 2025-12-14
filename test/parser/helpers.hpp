#pragma once

#include "assert_visitor.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <print>
#include <vector>

namespace loxmocha::test::helpers {

template<typename... Args>
[[nodiscard]] inline auto d(Args&&... decl) -> loxmocha::safe_ptr<loxmocha::decl::decl_t>
{
    return loxmocha::safe_ptr<loxmocha::decl::decl_t>::make("", std::forward<Args>(decl)...);
}

template<typename... Args>
[[nodiscard]] inline auto e(Args&&... expr) -> loxmocha::safe_ptr<loxmocha::expr::expr_t>
{
    return loxmocha::safe_ptr<loxmocha::expr::expr_t>::make("", std::forward<Args>(expr)...);
}

template<typename... Args>
[[nodiscard]] inline auto p(Args&&... pattern) -> loxmocha::safe_ptr<loxmocha::pattern::pattern_t>
{
    return loxmocha::safe_ptr<loxmocha::pattern::pattern_t>::make("", std::forward<Args>(pattern)...);
}

template<typename... Args>
[[nodiscard]] inline auto s(Args&&... stmt) -> loxmocha::safe_ptr<loxmocha::stmt::stmt_t>
{
    return loxmocha::safe_ptr<loxmocha::stmt::stmt_t>::make("", std::forward<Args>(stmt)...);
}

template<typename... Args>
[[nodiscard]] inline auto t(Args&&... args) -> loxmocha::safe_ptr<loxmocha::type::type_t>
{
    return loxmocha::safe_ptr<loxmocha::type::type_t>::make("", std::forward<Args>(args)...);
}

template<typename T, typename... Args>
auto make_vector(Args&&... args) -> std::vector<T>
{
    std::vector<T> vec;
    vec.reserve(sizeof...(Args));
    (vec.emplace_back("", std::forward<Args>(args)), ...);
    return vec;
}

template<typename T, typename... Args>
auto make_vector2(Args&&... args) -> std::vector<T>
{
    std::vector<T> vec;
    vec.reserve(sizeof...(Args));
    (vec.emplace_back(std::forward<Args>(args)), ...);
    return vec;
}

void base_test(const std::string& source, const auto& expected, auto&& parse)
{
    // NOLINTNEXTLINE(misc-const-correctness)
    lexer_t lexer{source};
    auto    result = parse(lexer);

    EXPECT_TRUE(!!result) << "Parsing has failed with an error";

    for (const auto& diag : result.diagnostics()) {
        std::print("Diagnostic: {}\n", diag);
    }

    result.result().visit(assert_visitor{}, expected);
}

inline void decl_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_decl); }

inline void expr_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_expr); }

inline void stmt_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_stmt); }

inline void type_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_type); }

inline void
rainy_day_test(const std::string& source, const std::vector<std::string>& expected_diagnostics, auto&& parse)
{
    // NOLINTNEXTLINE(misc-const-correctness)
    lexer_t lexer{source};
    auto    result = parse(lexer);

    EXPECT_FALSE(!!result) << "Parsing was expected to fail but succeeded";

    EXPECT_EQ(result.diagnostics().size(), expected_diagnostics.size())
        << "Number of diagnostics does not match expected";

    for (const auto& [a_diag, e_diag] : std::views::zip(result.diagnostics(), expected_diagnostics)) {
        EXPECT_EQ(a_diag, e_diag) << "Diagnostic message does not match expected. Expected: " << e_diag
                                  << ", Actual: " << a_diag;
    }
}

inline void rainy_day_expr_test(const std::string& source, const std::vector<std::string>& expected_diagnostics)
{
    rainy_day_test(source, expected_diagnostics, parse_expr);
}

} // namespace loxmocha::test::helpers
