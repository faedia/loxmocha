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

[[nodiscard]] inline auto e(loxmocha::expr::expr_t&& expr) -> loxmocha::safe_ptr<loxmocha::expr::expr_t>
{
    return loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(std::move(expr));
}

[[nodiscard]] inline auto p(loxmocha::pattern::pattern_t&& pattern) -> loxmocha::safe_ptr<loxmocha::pattern::pattern_t>
{
    return loxmocha::safe_ptr<loxmocha::pattern::pattern_t>::make(std::move(pattern));
}

[[nodiscard]] inline auto t(loxmocha::type::type_t&& type) -> loxmocha::safe_ptr<loxmocha::type::type_t>
{
    return loxmocha::safe_ptr<loxmocha::type::type_t>::make(std::move(type));
}

template<typename T, typename... Args>
auto make_vector(Args&&... args) -> std::vector<T>
{
    std::vector<T> vec;
    vec.reserve(sizeof...(Args));
    (vec.emplace_back(std::forward<Args>(args)), ...);
    return vec;
}

template<typename T>
void base_test(const std::string& source, const T& expected, auto&& parse)
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

inline void expr_test(const std::string& source, const expr::expr_t& expected)
{
    base_test<expr::expr_t>(source, expected, parse_expr);
}

inline void type_test(const std::string& source, const type::type_t& expected)
{
    base_test<type::type_t>(source, expected, parse_type);
}

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
